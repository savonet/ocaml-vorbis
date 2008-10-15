(*
 Copyright 2003 Savonet team

 This file is part of OCaml-Vorbis.

 OCaml-Vorbis is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 OCaml-Vorbis is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with OCaml-Vorbis; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  An ogg to wav converter using OCaml-Vorbis.

  @author Samuel Mimram
  *)

(* $Id$ *)

let bufsize = 16 * 1024

let src = ref ""
let dst = ref ""


open Unix


let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))


let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))


let progress_bar =
  let spin = ref 0 in
    (
      fun title pos tot ->
        let nbeq = 40 in
        let n = min (100. *. (float_of_int pos) /. (float_of_int tot)) 100. in
        let e = int_of_float (n /. 100. *. (float_of_int nbeq)) in
          Printf.printf "\r%s %6.2f%% [" title n;
          for i = 1 to e do Printf.printf "=" done;
          if e != nbeq then Printf.printf ">";
          for i = e + 2 to nbeq do Printf.printf " " done;
          Printf.printf "] ";
          incr spin;
          if !spin > 4 then spin := 1;
          Printf.printf "%c%!"
            (
              if n = 100. then ' '
              else
                match !spin with
                  | 1 -> '|'
                  | 2 -> '/'
                  | 3 -> '-'
                  | 4 -> '\\'
                  | _ -> failwith "this did not happen"
            )
    )


let usage = "usage: ogg2wav [options] source destination"


let _ =
  Arg.parse
    []
    (
      let pnum = ref (-1) in
        (fun s -> incr pnum; match !pnum with
           | 0 -> src := s
           | 1 -> dst := s
           | _ -> Printf.eprintf "Error: too many arguments\n"; exit 1
        )
    ) usage;
  if !src = "" || !dst = "" then
    (
      Printf.printf "%s\n" usage;
      exit 1
    );

  (* Using vorbis to decode the ogg. *)
  let params =
    {
      Vorbis.sample_size = 16;
      Vorbis.big_endian = false;
      Vorbis.signed = true
    }
  in
  let tmpdst, oc = Filename.open_temp_file ~mode:[Open_binary] "ogg2wav" ".raw" in
  let srcf = Unix.openfile !src [Unix.O_RDONLY] 0o400 in
  let read_func n =
    let buf = String.create n in
      ignore (Unix.read srcf buf 0 n);
      buf
  in
  let seek_func () = -1 in
  let close_func () =
    Unix.close srcf
  in
  let tell_func () = -1 in
  let df = Vorbis.open_dec_stream read_func seek_func close_func tell_func params in
    (
      let infos = Vorbis.get_dec_file_info df in
      let vdr, cmt = Vorbis.get_dec_file_comments df None in
        Printf.printf "Input file characteristics: vorbis codec v%d, %d channel(s), %d Hz, %d s\n" infos.Vorbis.vorbis_version infos.Vorbis.audio_channels infos.Vorbis.audio_sample_rate infos.Vorbis.duration;
        Printf.printf "* vendor: %s\n" vdr;
        Array.iter (fun (c, v) -> Printf.printf "* %s: %s\n" c v) cmt;
        Printf.printf "\n";

        let buf = String.create bufsize in
        let pos = ref 0 in
        let tot = infos.Vorbis.duration * 44100 * 2 * 2 in
          try
            while true
            do
              let r = Vorbis.decode df buf 0 bufsize in
                output oc buf 0 r;
                pos := !pos + r;
                progress_bar "Decoding ogg:" !pos tot
            done;
            close_out oc; Vorbis.close_dec_file df
          with
            | Vorbis.Hole_in_data ->
                Printf.printf "\nHole in the data!\n";
                close_out oc;
                Vorbis.close_dec_file df
            | End_of_file -> close_out oc; Vorbis.close_dec_file df
    );
    Printf.printf "\n";

    (* Do the wav stuff. *)
    let datalen = (stat tmpdst).st_size in
    let ic = open_in_bin tmpdst in
    let oc = open_out_bin !dst in
      output_string oc "RIFF";
      output_int oc (4 + 24 + 8 + datalen);
      output_string oc "WAVE";
      output_string oc "fmt ";
      output_int oc 16;
      output_short oc 1; (* WAVE_FORMAT_PCM *)
      output_short oc 2; (* channels *)
      output_int oc 44100; (* freq *)
      output_int oc (44100 * 2 * 2); (* bytes / s *)
      output_short oc (2 * 2); (* block alignment *)
      output_short oc 16; (* bits per sample *)
      output_string oc "data";
      output_int oc datalen;
      (
        let buflen = 256 * 1024 in
        let buf = String.create buflen in
        let r = ref 1 in
        let pos = ref 0 in
        let tot = datalen in
          while !r <> 0
          do
            r := input ic buf 0 buflen;
            output oc buf 0 !r;
            pos := !pos + !r;
            progress_bar "Tagging wav: " !pos tot
          done
      );
      close_in ic;
      close_out oc;
      Unix.unlink tmpdst;
      Printf.printf "\n"

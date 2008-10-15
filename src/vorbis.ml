(*
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of Ocaml-vorbis.
 *
 * Ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Decode from or encode to the Ogg Vorbis compressed audio format; or get
  * informations about an Ogg Vorbis file.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

type encoder

exception Invalid_parameters
exception Invalid_quality
exception Invalid_bitrate
exception Invalid_samplesize
exception Invalid_channels
exception Invalid_sample_freq
exception Could_not_open_file
exception Not_vorbis
exception Hole_in_data
exception Bad_link
exception Version_mismatch
exception Bad_header
exception Read_error
exception Internal_fault
exception Invalid_argument
exception Not_implemented
exception Unknown_error of int
exception Not_audio
exception False
exception Utf8_failure of string

let _ =
  Callback.register_exception "vorbis_exn_invalid_parameters" Invalid_parameters;
  Callback.register_exception "vorbis_exn_invalid_quality" Invalid_quality;
  Callback.register_exception "vorbis_exn_invalid_bitrate" Invalid_bitrate;
  Callback.register_exception "vorbis_exn_invalid_samplesize" Invalid_samplesize;
  Callback.register_exception "vorbis_exn_invalid_channels" Invalid_channels;
  Callback.register_exception "vorbis_exn_invalid_sample_freq" Invalid_sample_freq;
  Callback.register_exception "vorbis_exn_could_not_open_file" Could_not_open_file;
  Callback.register_exception "vorbis_exn_not_vorbis" Not_vorbis;
  Callback.register_exception "vorbis_exn_hole_in_data" Hole_in_data;
  Callback.register_exception "vorbis_exn_bad_link" Bad_link;
  Callback.register_exception "vorbis_exn_version_mismatch" Version_mismatch;
  Callback.register_exception "vorbis_exn_bad_header" Bad_header;
  Callback.register_exception "vorbis_exn_read_error" Read_error;
  Callback.register_exception "vorbis_exn_internal_fault" Internal_fault;
  Callback.register_exception "vorbis_exn_invalid" Invalid_argument;
  Callback.register_exception "vorbis_exn_not_implemented" Not_implemented;
  Callback.register_exception "vorbis_exn_not_audio" Not_audio;
  Callback.register_exception "vorbis_exn_unknown_error" (Unknown_error 0);
  Callback.register_exception "vorbis_exn_false" False;
  Callback.register_exception "vorbis_exn_utf8_failure" (Utf8_failure "")

let tags ?title ?artist ?genre ?date ?album ?tracknumber ?comment () =
  let ans = ref [] in
  let add t v =
    match v with
      | Some v -> ans := (t, v) :: !ans
      | None -> ()
  in
    add "ARTIST" artist;
    add "TITLE" title;
    add "ALBUM" album;
    add "GENRE" genre;
    add "DATE" date;
    add "TRACKNUMBER" tracknumber;
    add "COMMENT" comment;
    List.rev !ans

let encoder_tag = "ocaml-vorbis by the savonet team (http://savonet.sf.net/)"

module Encoder =
struct
  type t

  external create : int -> int -> int -> int -> int -> t = "ocaml_vorbis_analysis_init"

  external create_vbr : int -> int -> float -> t = "ocaml_vorbis_analysis_init_vbr"

  external reset : t -> unit = "ocaml_vorbis_reset"

  external headerout : t -> Ogg.Stream.t -> (string * string) array -> unit = "ocaml_vorbis_analysis_headerout"

  let headerout state os tags =
    let tags = Array.of_list (tags@[("ENCODER", encoder_tag)]) in
      headerout state os tags

  external encode_buffer_float : t -> Ogg.Stream.t -> float array array -> int -> int -> unit = "ocaml_vorbis_encode_float"

  let end_of_stream enc os =
    encode_buffer_float enc os [||] 0 0
end

let split_comment comment =
  try
    let equal_pos =
      String.index_from comment 0 '='
    in
    let c1 =
      String.uppercase (String.sub comment 0 equal_pos)
    in
    let c2 =
      String.sub comment (equal_pos + 1) ((String.length comment) - equal_pos - 1)
    in
      c1, c2;
  with Not_found -> comment, ""

type bitstream = int

type info = {
  vorbis_version : int;
  audio_channels : int;
  audio_samplerate : int;
  bitrate_upper : int;
  bitrate_nominal : int;
  bitrate_lower : int;
  bitrate_window : int;
}

let opt_bs = function
  | Some bs -> bs
  | None -> -1

module File = 
struct
  module Decoder =
  struct
    type t
  
    external create : (int -> string * int) -> (int -> Unix.seek_command -> int) -> (unit -> unit) -> (unit -> int) -> t = "ocaml_vorbis_open_dec_stream"
  
    let openfile_with_fd f =
      let fd = Unix.openfile f [Unix.O_RDONLY] 0o400 in
        try
          create
            (fun n ->
               let buf = String.create n in
               let r = Unix.read fd buf 0 n in
                 buf, r)
            (fun n cmd -> Unix.lseek fd n cmd)
            (fun () -> Unix.close fd)
            (fun () -> Unix.lseek fd 0 Unix.SEEK_CUR), fd
        with
          | e ->
              Unix.close fd;
              raise e
  
    let openfile f = fst (openfile_with_fd f)
  
    external decode_float : t -> float array array -> int -> int -> int = "ocaml_vorbis_decode_float"
  
    external decode_float_alloc : t -> int -> float array array = "ocaml_vorbis_decode_float_alloc"
  
    external decode : t -> bool -> int -> bool -> string -> int -> int -> int = "ocaml_vorbis_decode_byte" "ocaml_vorbis_decode"
  
    let decode df ?(big_endian=false) ?(sample_size=2) ?(signed=true) buf ofs len =
      decode df big_endian sample_size signed buf ofs len
  
    external close : t -> unit = "ocaml_vorbis_close_dec_file"
  
    external bitstream : t -> int = "ocaml_vorbis_get_dec_file_bitstream"
  
    external comments : t -> int -> string * (string array) = "ocaml_vorbis_get_dec_file_comments"
  
    let comments df bitstream =
      let vd, cmts = comments df bitstream in
        vd, (Array.to_list (Array.map split_comment cmts))
  
    external info : t -> int -> info = "ocaml_vorbis_decoder_info"
  
    external bitrate : t -> int -> int = "ocaml_vorbis_decoder_bitrate"
  
    external duration : t -> int -> float = "ocaml_vorbis_decoder_time_total"
  
    external streams : t -> int = "ocaml_vorbis_decoder_streams"
  
    external serialnumber : t -> int -> int = "ocaml_vorbis_decoder_serialnumber"
  
    external samples : t -> int -> int = "ocaml_vorbis_decoder_pcm_total"
  end
end

module Decoder =
struct

  type t

  external init : Ogg.Stream.packet -> Ogg.Stream.packet -> Ogg.Stream.packet -> t = "ocaml_vorbis_synthesis_init"

  external info : t -> info = "ocaml_vorbis_val_info_of_decoder"

  external comments : t -> string * (string array) = "ocaml_vorbis_val_comments_of_decoder" 

  let comments dec =
    let vend,cmts = comments dec in
      vend,Array.to_list (Array.map split_comment cmts)

  external check_packet : Ogg.Stream.packet -> bool = "ocaml_vorbis_check_packet"

  external decode_pcm : t -> Ogg.Stream.t -> float array array -> int -> int -> int = "ocaml_vorbis_decode_pcm"

  let decode_pcm dec os buf pos len = 
    let ret = decode_pcm dec os buf pos len in
    if ret = 0 then
      raise Ogg.Not_enough_data
    else
      ret

  external restart : t -> unit = "ocaml_vorbis_synthesis_restart"
  
end

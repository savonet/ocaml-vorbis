
let _ = Vorbis.set_charset "UTF-8"

let file = Sys.argv.(1)

let _ =
  Array.iter (fun (k,v) -> Printf.printf "%s: %s\n" k v)
    (snd (Vorbis.get_comments file))

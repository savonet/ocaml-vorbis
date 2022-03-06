let () =
  let sum1 = Digest.file Sys.argv.(1) in
  let sum2 = Digest.file Sys.argv.(2) in
  if sum1 <> sum2 then exit 1

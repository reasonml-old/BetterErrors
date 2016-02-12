let get_match_n n pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) n
(* get the first (presumably only) match in a string *)
let get_match = get_match_n 1

let get_match_maybe pat str =
  try Some (Pcre.get_substring (Pcre.exec ~pat:pat str) 1)
  with Not_found -> None

let split sep str = Pcre.split ~pat:sep str

let rec splitInto ~chunckSize (l: 'a list): 'a list list =
  if BatList.length l <= chunckSize || chunckSize = 0 then [l]
  else (BatList.take chunckSize l) :: (splitInto ~chunckSize (BatList.drop chunckSize l))

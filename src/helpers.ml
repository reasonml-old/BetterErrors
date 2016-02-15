let get_match_n n pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) n
(* get the first (presumably only) match in a string *)
let get_match = get_match_n 1

let get_match_maybe pat str =
  try Some (Pcre.get_substring (Pcre.exec ~pat:pat str) 1)
  with Not_found -> None

let get_match_n_maybe n pat str =
  try Some (Pcre.get_substring (Pcre.exec ~pat:pat str) n)
  with _ -> None

let execMaybe pat str =
  try Some (Pcre.exec ~pat:pat str)
  with Not_found -> None

let getSubstringMaybe result n =
  try Some (Pcre.get_substring result n)
  with Not_found -> None

let split sep str = Pcre.split ~pat:sep str

let rec splitInto ~chunckSize (l: 'a list): 'a list list =
  if BatList.length l <= chunckSize || chunckSize = 0 then [l]
  else (BatList.take chunckSize l) :: (splitInto ~chunckSize (BatList.drop chunckSize l))

let red = ANSITerminal.sprintf [ANSITerminal.red] "%s"
let redUnderlined = ANSITerminal.sprintf [ANSITerminal.red; ANSITerminal.Underlined] "%s"
let yellow = ANSITerminal.sprintf [ANSITerminal.yellow] "%s"
let yellowUnderlined = ANSITerminal.sprintf [ANSITerminal.yellow; ANSITerminal.Underlined] "%s"
let green = ANSITerminal.sprintf [ANSITerminal.green] "%s"

let mapcat sep f l = BatString.concat sep (BatList.map f l)

let sp = Printf.sprintf

let highlight ?(color=red) ?(first=0) ?(last=99999) str =
  (BatString.slice ~last:first str)
    ^ (color @@ BatString.slice ~first ~last str)
    ^ (BatString.slice ~first:last str)

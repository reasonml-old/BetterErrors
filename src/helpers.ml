let get_match_n n pat str =
  let rex = Re_pcre.regexp pat in
  Re_pcre.get_substring (Re_pcre.exec ~rex str) n
(* get the first (presumably only) match in a string *)
let get_match = get_match_n 1

let get_match_maybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) 1)
  with Not_found -> None

let get_match_n_maybe n pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) n)
  with _ -> None

let execMaybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.exec ~rex str)
  with Not_found -> None

let getSubstringMaybe result n =
  try Some (Re_pcre.get_substring result n)
  with Not_found -> None

let split sep str =
  let rex = Re_pcre.regexp sep in
  Re_pcre.split ~rex str

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

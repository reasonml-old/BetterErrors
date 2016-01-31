open Types

let filenameR = {|File "(.+)"|}
let lineR = {|File .+, line (\d+)|}
let chars1R = {|File .+, characters (\d+)|}
let chars2R = {|File .+, characters .+-(\d+)|}

(* helper for getting the first (presumably only) match in a string *)
let get_match pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) 1

let extractor1 err errLines =
  try
    let filename = get_match filenameR err in
    let fileLines = Batteries.List.of_enum (BatFile.lines_of filename)  in
    let line = get_match lineR err in
    let chars1 = get_match chars1R err in
    let chars2 = get_match chars2R err in
      TypeError (
        filename ^ " " ^ line ^ ":" ^ chars1 ^ "-" ^ chars2 ^ "\n" ^
        (List.nth fileLines ((int_of_string line) - 1)) ^ "\n" ^
        (String.make (int_of_string chars1) ' ') ^
        (String.make ((int_of_string chars2) - (int_of_string chars1)) '^') ^ "\n" ^
        String.concat "\n" (List.tl errLines)
      )
  with Not_found -> Unparsable err

let () =
  (* read stdin til end *)
  let err = BatIO.nread BatIO.stdin 99999 in
    Reporter.print (extractor1 err (Pcre.split ~pat:"\n" err))

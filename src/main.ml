let lines_of_file filename =
  let lines = ref [] in
  let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines

(* ---------------- *)

(* let () = print_int (Array.length Sys.argv) *)
(* 0: script name, 1: first arg (if any) *)
(* let () = print_endline Sys.argv.(0);; *)


(* ------------- actual code ------------- *)
let filenameR = {|File "(.+)"|}
let lineR = {|File .+, line ([0-9]+)|}
let chars1R = {|File .+, characters ([0-9]+)|}
let chars2R = {|File .+, characters .+-([0-9]+)|}

(* helper for getting the first (presumably only) match in a string *)
let get_match pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) 1

let extractor1 err errLines =
  try
    let filename = get_match filenameR err in
    let fileLines = lines_of_file filename in
    let line = get_match lineR err in
    let chars1 = get_match chars1R err in
    let chars2 = get_match chars2R err in
      Some (
        filename ^ " " ^ line ^ ":" ^ chars1 ^ "-" ^ chars2 ^ "\n" ^
        (List.nth fileLines ((int_of_string line) - 1)) ^ "\n" ^
        (String.make (int_of_string chars1) ' ') ^
        (String.make ((int_of_string chars2) - (int_of_string chars1)) '^') ^ "\n" ^
        String.concat "\n" (List.tl errLines)
      )
  with Not_found -> None

let () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done;
  with End_of_file ->
    let errLines = List.rev !lines in
    let err = String.concat "\n" errLines in
      match extractor1 err errLines with
        | Some msg -> print_endline msg
        | None -> print_endline "couldn't parse error, original:"; print_endline err

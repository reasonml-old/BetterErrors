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
let filenameR = Str.regexp {|File "\(.+\)"|}
let lineR = Str.regexp {|File .+, line \([0-9]+\)|}
let chars1R = Str.regexp {|File .+, characters \([0-9]+\)|}
let chars2R = Str.regexp {|File .+, characters .+-\([0-9]+\)|}

let extractor1 err errLines =
  try
    let filename = ignore (Str.string_match filenameR err 0); Str.matched_group 1 err in
    let fileLines = lines_of_file filename in
    let line = ignore (Str.string_match lineR err 0); Str.matched_group 1 err in
    let chars1 = ignore (Str.string_match chars1R err 0); Str.matched_group 1 err in
    let chars2 = ignore (Str.string_match chars2R err 0); Str.matched_group 1 err in
      Some (
        filename ^ " " ^ line ^ ":" ^ chars1 ^ "-" ^ chars2 ^ "\n" ^
        (List.nth fileLines ((int_of_string line) - 1)) ^ "\n" ^
        (String.make (int_of_string chars1) ' ') ^
        (String.make ((int_of_string chars2) - (int_of_string chars1)) '^') ^ "\n" ^
        String.concat "\n" (List.tl errLines)
      )
  with Invalid_argument _ -> None

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

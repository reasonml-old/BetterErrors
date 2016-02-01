open Types

let highlightFile {name ; line ; cols = (chars1, chars2)} =
  let fileLines = Batteries.List.of_enum (BatFile.lines_of name) in
    (Printf.sprintf "%s %d:%d-%d\n" name line chars1 chars2) ^
    (List.nth fileLines (line - 1)) ^ "\n" ^
    (String.make chars1 ' ') ^
    (String.make (chars2 - chars1) '^')

let print msg = match msg with
  | Unparsable err -> print_endline "couldn't parse error, original:"; print_endline err
  | Type_MismatchTypeArguments err -> print_endline err.constructor
  | Type_IncompatibleType {fileInfo; inferred; expected} ->
    print_string (highlightFile fileInfo ^ " ");
    print_endline ("wanted " ^ expected ^ ", got " ^ inferred ^ " instead!")
  | _ -> print_endline "huh"

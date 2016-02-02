open Types

let highlightFile {name ; line ; cols = (chars1, chars2)} =
  let fileLines = Batteries.List.of_enum (BatFile.lines_of name) in
    (Printf.sprintf "%s:%d %d-%d\n" name line chars1 chars2) ^
    (List.nth fileLines (line - 1)) ^ "\n" ^
    (String.make chars1 ' ') ^
    (String.make (chars2 - chars1) '^')

let print msg = match msg with
  | Unparsable err -> print_endline "couldn't parse error, original:"; print_endline err
  | Type_MismatchTypeArguments err -> print_endline err.constructor
  | Type_IncompatibleType {fileInfo; actual; expected} ->
    print_string (highlightFile fileInfo ^ " ");
    print_endline ("this is " ^ actual ^ ", wanted " ^ expected ^ " instead.")
  | Type_NotAFunction {fileInfo; actual} ->
    print_string (highlightFile fileInfo ^ " ");
    print_endline ("this is " ^ actual ^ ". You seem to have called it as a function.");
    print_endline "Careful with the spaces and the parentheses, and whatever's in-between!"
  | Type_AppliedTooMany {fileInfo; functionType; expectedArgCount} ->
    print_string (highlightFile fileInfo ^ " ");
    print_endline ("this function has type " ^ functionType);
    Printf.printf "It accepts only %d arguments. You gave more. " expectedArgCount;
    print_endline "Maybe you forgot a `;` somewhere?"
  | File_SyntaxError {fileInfo} ->
    print_string (highlightFile fileInfo ^ " ");
    print_endline "the syntax's' wrong.";
    print_endline "Note: the location indicated might not be accurate."
  | File_IllegalCharacter {fileInfo; character} ->
    print_string (highlightFile fileInfo ^ " ");
    Printf.printf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!\n" character
  | _ -> print_endline "huh"

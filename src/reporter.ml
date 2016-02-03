open Types

type lineHighlight = {
  line: int;
  cols: int * int;
}

let numberOfDigits n =
  let digits = ref 1 in
  let nn = ref n in
  while !nn / 10 > 0 do
    nn := !nn / 10;
    digits := !digits + 1
  done;
  !digits

let pad ?(ch=' ') content n =
  (BatString.make (n - (BatString.length content)) ch) ^ content

let _printFile ?(sep=" | ") ~highlight:{line; cols = (chars1, chars2)} content =
  let startIndex = max 0 (line - 3) in
  let endIndex = min (List.length content - 1) (line + 3) in
  let lineNumWidth = numberOfDigits (List.length content) in
  let result = ref "" in
  for i = startIndex to endIndex do
    result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ (List.nth content i) ^ "\n";
    if i = line - 1 then
      result := !result ^ (String.make (chars1 + lineNumWidth + BatString.length sep) ' ')
        ^ (String.make (chars2 - chars1) '^') ^ "\n"
    else ()
  done;
  !result

let printFile {content; name; line; cols = (chars1, chars2)} =
  (Printf.sprintf "%s:%d %d-%d\n" name line chars1 chars2) ^
  (_printFile
    ~sep: " | "
    ~highlight: {line = line; cols = (chars1, chars2)}
    content
  )

let print msg = match msg with
  | Unparsable err -> print_endline "couldn't parse error, original:"; print_endline err
  | Type_MismatchTypeArguments {fileInfo; typeConstructor; expectedCount; actualCount} ->
    print_string (printFile fileInfo ^ " ");
    Printf.printf "This needs to be applied to %d argument(s), we found %d.\n" expectedCount actualCount
  | Type_IncompatibleType {fileInfo; actual; expected} ->
    print_string (printFile fileInfo ^ " ");
    print_endline ("this is " ^ actual ^ ", wanted " ^ expected ^ " instead.")
  | Type_NotAFunction {fileInfo; actual} ->
    print_string (printFile fileInfo ^ " ");
    print_endline ("this is " ^ actual ^ ". You seem to have called it as a function.");
    print_endline "Careful with the spaces and the parentheses, and whatever's in-between!"
  | Type_AppliedTooMany {fileInfo; functionType; expectedArgCount} ->
    print_string (printFile fileInfo ^ " ");
    print_endline ("this function has type " ^ functionType);
    Printf.printf "It accepts only %d arguments. You gave more. " expectedArgCount;
    print_endline "Maybe you forgot a `;` somewhere?"
  | File_SyntaxError {fileInfo} ->
    print_string (printFile fileInfo ^ " ");
    print_endline "the syntax's' wrong.";
    print_endline "Note: the location indicated might not be accurate."
  | File_IllegalCharacter {fileInfo; character} ->
    print_string (printFile fileInfo ^ " ");
    Printf.printf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!\n" character
  | Type_UnboundTypeConstructor {fileInfo; namespacedConstructor; suggestion} ->
    print_string (printFile fileInfo ^ " ");
    print_endline ("The type constructor " ^ namespacedConstructor ^ " can't be found.");
    (match suggestion with
    | None -> ()
    | Some h -> print_endline ("Hint: did you mean `" ^ h ^ "`?"))
  | Warning_PatternNotExhaustive {fileInfo; unmatched; warningCode} ->
    print_string (printFile fileInfo ^ " ");
    Printf.printf "Warning %d: this match doesn't cover all possible values of the variant.\n" warningCode;
    (match unmatched with
    | [oneVariant] -> print_endline @@ "The case `" ^ oneVariant ^ "` is not matched"
    | many ->
        print_endline "These cases are not matched:";
        List.iter (fun x -> print_endline @@ "- `" ^ x ^ "`") many)
  | _ -> print_endline "huh"

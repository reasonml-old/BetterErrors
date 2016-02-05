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
  let startIndex = max 0 (line - 4) in
  let endIndex = min (List.length content - 1) (line + 2) in
  let lineNumWidth = numberOfDigits (List.length content) in
  let highlightLength = chars2 - chars1 in
  let result = ref "" in
  for i = startIndex to endIndex do
    if i = line - 1 then
      let currLine = List.nth content i in
      result := !result ^ (Printf.sprintf "%s" (pad (string_of_int (i + 1)) lineNumWidth)) ^ sep ^
        (* TODO: see warning_PatternNotExhaustive for error concerning sub *)
        (BatString.sub currLine 0 chars1) ^ (ANSITerminal.sprintf [ANSITerminal.red] "%s" (BatString.sub currLine chars1 highlightLength)) ^ (BatString.sub currLine chars2 ((BatString.length currLine) - chars2)) ^ "\n";
      result := !result ^ (String.make (chars1 + lineNumWidth + BatString.length sep) ' ') ^
        (String.make highlightLength '^') ^ "\n"
    else
      result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ (List.nth content i) ^ "\n"
  done;
  !result

let printFile {content; name; line; cols = (chars1, chars2)} =
  (ANSITerminal.sprintf [ANSITerminal.cyan] "%s:%d %d-%d\n" name line chars1 chars2) ^
  (_printFile
    ~sep: " | "
    ~highlight: {line = line; cols = (chars1, chars2)}
    content
  )

let print msg = match msg with
  (* let's handle the special cases first *)
  | NoErrorNorWarning err ->
    print_endline err;
    ANSITerminal.printf [ANSITerminal.green] "%s\n" "✔ Seems fine!"
  | UnparsableButWithFileInfo {fileInfo; error} ->
    print_endline @@ printFile fileInfo;
    print_string error
  | Unparsable err ->
    print_endline err;
    ANSITerminal.printf [ANSITerminal.red] "%s.\n" "✘ Couldn't parse error."

  (* normal cases now! *)
  | Type_MismatchTypeArguments {fileInfo; typeConstructor; expectedCount; actualCount} ->
    print_endline @@ printFile fileInfo;
    Printf.printf "This needs to be applied to %d argument(s), we found %d.\n" expectedCount actualCount
  | Type_IncompatibleType {fileInfo; actual; expected} ->
    print_endline @@ printFile fileInfo;
    print_endline ("This is " ^ actual ^ ", wanted " ^ expected ^ " instead.")
  | Type_NotAFunction {fileInfo; actual} ->
    print_endline @@ printFile fileInfo;
    print_endline ("This is " ^ actual ^ ". You seem to have called it as a function.");
    print_endline "Careful with the spaces and the parentheses, and whatever's in-between!"
  | Type_AppliedTooMany {fileInfo; functionType; expectedArgCount} ->
    print_endline @@ printFile fileInfo;
    print_endline ("This function has type " ^ functionType);
    Printf.printf "It accepts only %d arguments. You gave more. " expectedArgCount;
    print_endline "Maybe you forgot a `;` somewhere?"
  | File_SyntaxError {fileInfo} ->
    print_endline @@ printFile fileInfo;
    print_endline "The syntax is wrong.";
    print_endline "Note: the location indicated might not be accurate."
  | File_IllegalCharacter {fileInfo; character} ->
    print_endline @@ printFile fileInfo;
    Printf.printf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!\n" character
  | Type_UnboundTypeConstructor {fileInfo; namespacedConstructor; suggestion} ->
    print_endline @@ printFile fileInfo;
    print_endline ("The type constructor " ^ namespacedConstructor ^ " can't be found.");
    (match suggestion with
    | None -> ()
    | Some h -> print_endline ("Hint: did you mean `" ^ h ^ "`?"))
  | Type_UnboundValue {fileInfo; unboundValue; suggestion} ->
    print_endline @@ printFile fileInfo;
    (match suggestion with
    | None -> print_endline ("`" ^ unboundValue ^ "` can't be found. Could it be a typo?")
    | Some hint -> Printf.printf "`%s` can't be found. Did you mean `%s`?\n" unboundValue hint)
  | Type_UnboundRecordField {fileInfo; recordField; suggestion} ->
    print_endline @@ printFile fileInfo;
    (match suggestion with
    | None -> print_endline ("Field `" ^ recordField ^ "` can't be found in any declared types.")
    | Some hint -> print_endline ("Field `" ^ recordField ^ "` can't be found in any declared types. Did you mean `" ^ hint ^ "`?\n"))
  | Warning_PatternNotExhaustive {fileInfo; unmatched; warningCode} ->
    print_endline @@ printFile fileInfo;
    Printf.printf "Warning %d: this match doesn't cover all possible values of the variant.\n" warningCode;
    (match unmatched with
    | [oneVariant] -> print_endline @@ "The case `" ^ oneVariant ^ "` is not matched"
    | many ->
        print_endline "These cases are not matched:";
        List.iter (fun x -> print_endline @@ "- `" ^ x ^ "`") many)
  | Warning_OptionalArgumentNotErased {fileInfo} ->
    print_endline @@ printFile fileInfo;
    print_endline ("this optional argument cannot be erased.");
  | _ -> print_endline "huh"

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

let red = ANSITerminal.sprintf [ANSITerminal.red; ANSITerminal.Underlined] "%s"

let highlight ?first ?last str =
  (BatString.slice ?last:first str)
    ^ (red @@ BatString.slice ?first ?last str)
    ^ (BatString.slice ?first:last str)

(* row and col 0-indexed; endColumn is 1 past the actual end. See
Main.compilerLineColsToRange *)
let _printFile ?(sep=" | ") ~highlight:((startRow, startColumn), (endRow, endColumn)) content =
  let displayedStartRow = max 0 (startRow - 3) in
  (* we display no more than 3 lines after startRow. Some endRow are rly far
  away *)
  let displayedEndRow = min (BatList.length content - 1) (startRow + 3) in
  let lineNumWidth = numberOfDigits (BatList.length content) in
  let result = ref "" in
  for i = displayedStartRow to displayedEndRow do
    let currLine = BatList.at content i in
      if i >= startRow && i <= endRow then
        if startRow = endRow then
          result := !result ^ (Printf.sprintf "%s" (pad (string_of_int (i + 1)) lineNumWidth))
            ^ sep ^ (highlight ~first:startColumn ~last:endColumn currLine) ^ "\n"
        else if i = startRow then
          result := !result ^ (Printf.sprintf "%s" (pad (string_of_int (i + 1)) lineNumWidth))
            ^ sep ^ (highlight ~first:startColumn currLine) ^ "\n"
        else if i = endRow then
          result := !result ^ (Printf.sprintf "%s" (pad (string_of_int (i + 1)) lineNumWidth))
            ^ sep ^ (highlight ~last:endColumn currLine) ^ "\n"
        else
          result := !result ^ (Printf.sprintf "%s" (pad (string_of_int (i + 1)) lineNumWidth))
            ^ sep ^ (highlight currLine) ^ "\n"
      else
        result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ currLine ^ "\n"
  done;
  !result

let printFile {cachedContent; filePath} range =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  (ANSITerminal.sprintf
    [ANSITerminal.cyan]
    "%s:%d:%d-%d:%d\n"
    filePath
    (startRow + 1)
    startColumn
    (endRow + 1)
    endColumn
  ) ^ _printFile ~sep: " | " ~highlight:range cachedContent

let printAssumingErrorsAndWarnings l = l |> BatList.iter (fun {fileInfo; errors; warnings} ->
  errors |> BatList.iter (fun {range; parsedContent} -> match parsedContent with
    | Error_CatchAll error ->
      print_endline @@ printFile fileInfo range;
      print_string error
    | Type_MismatchTypeArguments {typeConstructor; expectedCount; actualCount} ->
      print_endline @@ printFile fileInfo range;
      Printf.printf "This needs to be applied to %d argument(s), we found %d.\n" expectedCount actualCount
    | Type_IncompatibleType {actual; expected} ->
      print_endline @@ printFile fileInfo range;
      print_endline ("This is " ^ actual ^ ", wanted " ^ expected ^ " instead.")
    | Type_NotAFunction {actual} ->
      print_endline @@ printFile fileInfo range;
      print_endline ("This is " ^ actual ^ ". You seem to have called it as a function.");
      print_endline "Careful with the spaces and the parentheses, and whatever's in-between!"
    | Type_AppliedTooMany {functionType; expectedArgCount} ->
      print_endline @@ printFile fileInfo range;
      print_endline ("This function has type " ^ functionType);
      Printf.printf "It accepts only %d arguments. You gave more. " expectedArgCount;
      print_endline "Maybe you forgot a `;` somewhere?"
    | File_SyntaxError {offendingString; hint} ->
      print_endline @@ printFile fileInfo range;
      print_endline @@ (match hint with
        | Some a -> "The syntax is wrong: " ^ a
        | None -> "The syntax is wrong.");
      (match offendingString with
        | ";" -> print_endline "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\")."
        | "else" -> print_endline @@ "Did you happen to have put a semicolon on the line before else?"
          ^ " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses."
        | _ -> ());
      print_endline "Note: the location indicated might not be accurate."
    | File_IllegalCharacter {character} ->
      print_endline @@ printFile fileInfo range;
      Printf.printf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!\n" character
    | Type_UnboundTypeConstructor {namespacedConstructor; suggestion} ->
      print_endline @@ printFile fileInfo range;
      print_endline ("The type constructor " ^ namespacedConstructor ^ " can't be found.");
      (match suggestion with
      | None -> ()
      | Some h -> print_endline ("Hint: did you mean `" ^ h ^ "`?"))
    | Type_UnboundValue {unboundValue; suggestion} ->
      print_endline @@ printFile fileInfo range;
      (match suggestion with
      | None -> print_endline ("`" ^ unboundValue ^ "` can't be found. Could it be a typo?")
      | Some hint -> Printf.printf "`%s` can't be found. Did you mean `%s`?\n" unboundValue hint)
    | Type_UnboundRecordField {recordField; suggestion} ->
      print_endline @@ printFile fileInfo range;
      (match suggestion with
      | None -> print_endline ("Field `" ^ recordField ^ "` can't be found in any record type.")
      | Some hint -> print_endline ("Field `" ^ recordField ^ "` can't be found in any record type. Did you mean `" ^ hint ^ "`?\n"))
    | Type_UnboundModule {unboundModule} ->
      print_endline @@ printFile fileInfo range;
      print_endline ("Module `" ^ unboundModule ^ "` not found in included libraries.");
      let pckName = BatString.lowercase unboundModule in
      print_endline (
        "Hint: your build rules might be missing a link. If you're using: \n" ^
        " - Oasis: make sure you have `"^ pckName ^"` under `BuildDepends` in your _oasis file.\n" ^
        " - ocamlbuild: make sure you have `-pkgs "^ pckName ^"` in your build command.\n" ^
        " - ocamlc | ocamlopt: make sure you have `-I +"^ pckName ^"` in your build command before the source files.\n" ^
        " - ocamlfind: make sure you have `-package "^ pckName ^" -linkpkg` in your build command.\n")
    |  _ -> print_endline "huh"
  );
  warnings |> BatList.iter (fun {range; parsedContent = {code; warningType}} -> match warningType with
    | Warning_CatchAll message ->
      print_endline @@ printFile fileInfo range;
      Printf.printf "Warning %d: %s\n" code message
    | Warning_PatternNotExhaustive {unmatched; warningCode} ->
      print_endline @@ printFile fileInfo range;
      Printf.printf "Warning %d: this match doesn't cover all possible values of the variant.\n" warningCode;
      (match unmatched with
      | [oneVariant] -> print_endline @@ "The case `" ^ oneVariant ^ "` is not matched"
      | many ->
          print_endline "These cases are not matched:";
          BatList.iter (fun x -> print_endline @@ "- `" ^ x ^ "`") many)
    | Warning_OptionalArgumentNotErased {warningCode; argumentName} ->
      print_endline @@ printFile fileInfo range;
      Printf.printf
        "Warning %d: %s is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n"
        warningCode
        argumentName
        argumentName;
      print_endline "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
    |  _ -> print_endline "huh"
  )
)

let print (content: result) = match content with
  (* handle the special cases first *)
  | NoErrorNorWarning err ->
    print_endline err;
    ANSITerminal.printf [ANSITerminal.green] "%s\n" "✔ Seems fine!"
  | Unparsable err ->
    print_endline err;
    ANSITerminal.printf [ANSITerminal.red] "%s\n" "✘ There might be an error."
  | ErrorsAndWarnings asd -> printAssumingErrorsAndWarnings asd

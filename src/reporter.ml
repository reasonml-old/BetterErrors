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
let redUnderlined = ANSITerminal.sprintf [ANSITerminal.red; ANSITerminal.Underlined] "%s"
let green = ANSITerminal.sprintf [ANSITerminal.green] "%s"

let highlight ?(first=0) ?(last=99999) str =
  (BatString.slice ~last:first str)
    ^ (redUnderlined @@ BatString.slice ~first ~last str)
    ^ (BatString.slice ~first:last str)

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
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~first:startColumn ~last:endColumn currLine) ^ "\n"
        else if i = startRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~first:startColumn currLine) ^ "\n"
        else if i = endRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~last:endColumn currLine) ^ "\n"
        else
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight currLine) ^ "\n"
      else
        result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ currLine ^ "\n"
  done;
  !result

let printFile {cachedContent; filePath} range =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  let fileInfo = if startRow = endRow then
      ANSITerminal.sprintf
        [ANSITerminal.cyan]
        "%s:%d %d-%d\n"
        filePath
        (startRow + 1)
        startColumn
        endColumn
    else
      ANSITerminal.sprintf
        [ANSITerminal.cyan]
        "%s:%d:%d-%d:%d\n"
        filePath
        (startRow + 1)
        startColumn
        (endRow + 1)
        endColumn
  in fileInfo ^ _printFile ~sep: " | " ~highlight:range cachedContent

let listify suggestions =
  suggestions
  |> BatList.map (fun sug -> "- `" ^ sug ^ "`")
  |> BatString.concat "\n"

let mapcat sep f l = BatString.concat sep (BatList.map f l)

let sp = Printf.sprintf

let decryptAssumingErrorsAndWarnings = mapcat "\n" (fun {fileInfo; errors; warnings} ->
  (errors |> mapcat "\n" (fun {range; parsedContent} ->
    let text = match parsedContent with
    | Error_CatchAll error -> error
    | Type_MismatchTypeArguments {typeConstructor; expectedCount; actualCount} ->
      sp "This needs to be applied to %d argument(s), we found %d." expectedCount actualCount
    | Type_IncompatibleType {actual; expected} ->
      Table.table
      ~align:Table.Left
      ~style:{
        Table.top = ("", "", "", "");
        Table.middle = ("", "", "", "");
        Table.bottom = ("", "", "", "");
        Table.vertical = ("", "  ", "");
      }
      ~padding:0
      [[red "This is:"; actual]; [green "Wanted:"; expected]]
    | Type_NotAFunction {actual} ->
      "This is " ^ actual ^ ". You seem to have called it as a function.\n"
        ^ "Careful with the spaces and the parentheses, and whatever's in-between!"
    | Type_AppliedTooMany {functionType; expectedArgCount} ->
      sp
        "This function has type %s\nIt accepts only %d arguments. You gave more. Maybe you forgot a `;` somewhere?"
        functionType
        expectedArgCount
    | File_SyntaxError {offendingString; hint} ->
      (match hint with
      | Some a -> "The syntax is wrong: " ^ a
      | None -> "The syntax is wrong.")
      ^ "\n" ^
      (match offendingString with
      | ";" -> "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\").\n"
      | "else" -> "Did you happen to have put a semicolon on the line before else?"
        ^ " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses.\n"
      | _ -> ""
      ) ^ "Note: the location indicated might not be accurate."
    | File_IllegalCharacter {character} ->
      sp "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!" character
    | Type_UnboundTypeConstructor {namespacedConstructor; suggestion} ->
      (sp "The type constructor %s can't be found." namespacedConstructor)
      ^
      (match suggestion with
      | None -> ""
      | Some h -> sp "\nHint: did you mean `%s`?" h)
    | Type_UnboundValue {unboundValue; suggestions} ->
      (match suggestions with
      | None -> sp "`%s` can't be found. Could it be a typo?" unboundValue
      | Some [hint] -> sp "`%s` can't be found. Did you mean `%s`?" unboundValue hint
      | Some [hint1; hint2] -> sp "`%s` can't be found. Did you mean `%s` or `%s`?" unboundValue hint1 hint2
      | Some hints -> sp "`%s` can't be found. Did you mean one of these?\n%s" unboundValue (listify hints))
    | Type_UnboundRecordField {recordField; suggestion} ->
      (match suggestion with
      | None -> sp "Field `%s` can't be found in any record type." recordField
      | Some hint -> sp "Field `%s` can't be found in any record type. Did you mean `%s`?" recordField hint)
    | Type_UnboundModule {unboundModule} ->
      (sp "Module `%s` not found in included libraries.\n" unboundModule)
      ^
      (let pckName = BatString.lowercase unboundModule in
      "Hint: your build rules might be missing a link. If you're using: \n" ^
      " - Oasis: make sure you have `"^ pckName ^"` under `BuildDepends` in your _oasis file.\n" ^
      " - ocamlbuild: make sure you have `-pkgs "^ pckName ^"` in your build command.\n" ^
      " - ocamlc | ocamlopt: make sure you have `-I +"^ pckName ^"` in your build command before the source files.\n" ^
      " - ocamlfind: make sure you have `-package "^ pckName ^" -linkpkg` in your build command.")
    |  _ -> "huh"
    in
    (printFile fileInfo range) ^ "\n" ^ text
  ))
  ^
  (warnings |> mapcat "\n" (fun {range; parsedContent = {code; warningType}} ->
    let text = match warningType with
    | Warning_CatchAll message ->
      sp "Warning %d: %s" code message
    | Warning_PatternNotExhaustive {unmatched} ->
      sp "Warning %d: this match doesn't cover all possible values of the variant.\n" code
      ^
      (match unmatched with
      | [oneVariant] -> sp "The case `%s` is not matched" oneVariant
      | many -> sp "These cases are not matched:\n%s" (mapcat "\n" (sp "- `%s`") many))
    | Warning_OptionalArgumentNotErased {argumentName} ->
      (sp
        "Warning %d: %s is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n"
        code
        argumentName
        argumentName)
        ^ "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
    |  _ -> "huh"
    in
    (printFile fileInfo range) ^ "\n" ^ text)
  )
)

let decryptCompilerMessages (content: result) = match content with
  (* handle the special cases first *)
  | NoErrorNorWarning err -> err ^ green "\n✔ Seems fine!"
  | Unparsable err -> err ^ red "\n✘ There might be an error."
  | ErrorsAndWarnings err -> decryptAssumingErrorsAndWarnings err

let print content = print_endline @@ decryptCompilerMessages content

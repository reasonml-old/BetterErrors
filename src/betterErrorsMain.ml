open BetterErrorsTypes
open Helpers

(* the compiler output might point to an error that spans across many lines;
however, instead of indicating from (startRow, startColumn) to (endRow,
endColumn), it'll indicate (startRow, startColumn, endColumn) where endColumn
might belong to a different row! We normalize and find the row here *)

(* the compiler line number is 1-indexed, and col number is 0-indexed but the
endColumn for an error goes past the last "expected" endColumn, e.g. if it's
`typ a = string`
instead of saying it's from 0 to 2, it shows as 0 to 3. This is also kinda
expected, since you get easy column count through 3 - 0 *)

(* we'll use 0-indexed. It's a reporter (printer)'s job to normalize to
1-indexed if it desires so *)
let normalizeCompilerLineColsToRange ~fileLines ~lineRaw ~col1Raw ~col2Raw =
  (* accept strings to constraint usage to parse directly from raw data *)
  let line = (int_of_string lineRaw) in
  let fileLength = BatList.length fileLines in
  let isOCamlBeingBadAndPointingToALineBeyondFileLength = line > fileLength in
  let (col1, col2) = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    let lastDamnReachableSpotInTheFile =
      BatString.length @@ BatList.at fileLines (fileLength - 1)
    in (lastDamnReachableSpotInTheFile - 1, lastDamnReachableSpotInTheFile)
  else
    match (col1Raw, col2Raw) with
    | (Some a, Some b) -> (int_of_string a, int_of_string b)
    (* some error msgs don't have column numbers; we normal them to 0 here *)
    | _ -> (0, 0)
  in
  let startRow = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    fileLength - 1
  else
    line - 1
  in
  let currentLine = BatList.at fileLines startRow in
  let numberOfCharsBetweenStartAndEndColumn = col2 - col1 in
  let numberOfCharsLeftToCoverOnStartingRow =
    (* +1 bc ocaml looooves to count new line as a char below when the error
    spans multiple lines*)
    (BatString.length currentLine) - col1 + 1
  in
  if numberOfCharsBetweenStartAndEndColumn <= numberOfCharsLeftToCoverOnStartingRow then
    ((startRow, col1), (startRow, col2))
  else
    let howManyCharsLeftToCoverOnSubsequentLines =
      ref (numberOfCharsBetweenStartAndEndColumn - numberOfCharsLeftToCoverOnStartingRow)
    in
    let suddenlyFunctionalProgrammingOutOfNowhere =
      fileLines
      |> BatList.drop (startRow + 1)
      |> BatList.map BatString.length
      |> BatList.take_while (fun numberOfCharsOnThisLine ->
        if !howManyCharsLeftToCoverOnSubsequentLines > numberOfCharsOnThisLine then
          (howManyCharsLeftToCoverOnSubsequentLines :=
            !howManyCharsLeftToCoverOnSubsequentLines - numberOfCharsOnThisLine - 1;
          true)
        else false)
    in
    let howManyMoreRowsCoveredSinceStartRow =
      1 + BatList.length suddenlyFunctionalProgrammingOutOfNowhere
    in
    ((startRow, col1),
    (startRow + howManyMoreRowsCoveredSinceStartRow, !howManyCharsLeftToCoverOnSubsequentLines))

(* has the side-effect of reading the file *)
let extractFromFileMatch fileMatch = Re_pcre.(
  match fileMatch with
  | [Delim _; Group (_, filePath); Group (_, lineNum); col1; col2; Text body] ->
    let cachedContent = BatList.of_enum (BatFile.lines_of filePath) in
    (* sometimes there's only line, but no characters *)
    let (col1Raw, col2Raw) = match (col1, col2) with
      | (Group (_, c1), Group (_, c2)) ->
        (* bug: https://github.com/mmottl/pcre-ocaml/issues/5 *)
        if BatString.trim c1 = "" || BatString.trim c2 = "" then (None, None)
        else (Some c1, Some c2)
      | _ -> (None, None)
    in
    (
      filePath,
      cachedContent,
      (normalizeCompilerLineColsToRange
        ~fileLines:cachedContent
        ~lineRaw:lineNum
        ~col1Raw:col1Raw
        ~col2Raw:col2Raw
      ),
      (* important, otherwise leaves random blank lines that defies some of
      our regex logic, maybe *)
      BatString.trim body
    )
  | _ -> raise (invalid_arg "Couldn't extract error")
)

(* debug helper *)
let printFullSplitResult = BatList.iteri (fun i x ->
  print_int i;
  print_endline "";
  Re_pcre.(
    match x with
    | Delim a -> print_endline @@ "Delim " ^ a
    | Group (_, a) -> print_endline @@ "Group " ^ a
    | Text a -> print_endline @@ "Text " ^ a
    | NoGroup -> print_endline @@ "NoGroup"
  )
)

let fileR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}

let hasErrorOrWarningR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  (* the all-caps ERROR is left by oasis when compilation fails bc of artifacts
  left in project folders *)
  {|^(Error|ERROR|Warning \d+): |}

let parse ~customErrorParsers err :result =
  let err = BatString.trim err in
  if not (Re_pcre.pmatch ~rex:hasErrorOrWarningR err) then NoErrorNorWarning err
  else
    let errorContent =
      err
      |> Re_pcre.full_split ~rex:fileR
      (* First few rows might be random output info *)
      |> BatList.drop_while (function Re_pcre.Text _ -> true | _ -> false)
    in
    if BatList.length errorContent = 0 then Unparsable err
    else
      err
      |> Re_pcre.full_split ~rex:fileR
      (* First few rows might be random output info *)
      |> BatList.drop_while (function Re_pcre.Text _ -> true | _ -> false)
      (* we match 6 items, so the whole list will always be a multiple of 6 *)
      |> splitInto ~chunckSize:6
      |> BatList.map extractFromFileMatch
      |> BatList.map (fun (filePath, cachedContent, range, body) ->
        let errorCapture = get_match_maybe {|^Error: ([\s\S]+)|} body in
        let warningCapture =
          match execMaybe {|^Warning (\d+): ([\s\S]+)|} body with
          | None -> (None, None)
          | Some capture -> (getSubstringMaybe capture 1, getSubstringMaybe capture 2)
        in
        match (errorCapture, warningCapture) with
        | (Some errorBody, (None, None)) ->
          Some (Error {
            filePath;
            cachedContent;
            range;
            parsedContent = BetterErrorsParseError.parse
              ~customErrorParsers
              ~errorBody
              ~cachedContent
              ~range;
          })
        | (None, (Some code, Some warningBody)) ->
          Some (Warning {
            filePath;
            cachedContent;
            range;
            parsedContent = {
              code = int_of_string code;
              warningType = ParseWarning.parse code warningBody cachedContent range;
            };
          })
        | _ -> None (* not an error, not a warning. False alarm? *)
      )
      |> BatList.filter_map BatPervasives.identity
      |> (fun x -> ErrorsAndWarnings x)

(* entry point, for convenience purposes for now. Theoretically the parser and
the reporters are decoupled *)
let parseFromStdin ~customErrorParsers =
  let err = BatPervasives.input_all stdin in
  try
    parse ~customErrorParsers err
    |> TerminalReporter.prettyPrintParsedResult
    |> print_endline
  with _ ->
    (* final fallback, just print  *)
    print_endline "Something went wrong during error parsing.";
    print_endline err

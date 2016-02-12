open Types
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
let extractFromFileMatch fileMatch: (fileInfo * Atom.Range.t * string) =
  Pcre.(
    match fileMatch with
    | [Delim _; Group (_, fileName); Group (_, lineNum); col1; col2; Text text] ->
      let cachedContent = BatList.of_enum (BatFile.lines_of fileName) in
      let (col1Raw, col2Raw) = match (col1, col2) with
        | (Group (_, c1), Group (_, c2)) -> (Some c1, Some c2)
        | _ -> (None, None)
      in
      (
        {filePath = fileName; cachedContent = cachedContent},
        (normalizeCompilerLineColsToRange
          ~fileLines:cachedContent
          ~lineRaw:lineNum
          ~col1Raw:col1Raw
          ~col2Raw:col2Raw
        ),
        (* important, otherwise leaves random blank lines that defies some of
        our regex logic, maybe *)
        BatString.trim text
      )
    | _ -> raise (invalid_arg "Couldn't extract error")
  )

(* debug helper *)
let printFullSplitResult = BatList.iteri (fun i x ->
  print_int i;
  print_endline "";
  Pcre.(
    match x with
    | Delim a -> print_endline @@ "Delim " ^ a
    | Group (_, a) -> print_endline @@ "Group " ^ a
    | Text a -> print_endline @@ "Text " ^ a
    | NoGroup -> print_endline @@ "NoGroup"
  )
)

let fileR = Pcre.regexp
  ~flags:[Pcre.(`MULTILINE)]
  {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}

let errorOrWarningR = Pcre.regexp
  ~flags:[Pcre.(`MULTILINE)]
  {|^(?:(?:Error)|(?:Warning) (\d+)): |}

let hasErrorOrWarningR = Pcre.regexp
  ~flags:[Pcre.(`MULTILINE)]
  (* the all-caps ERROR is left by oasis when compilation fails bc of artifacts
  left in project folders *)
  {|^(Error|ERROR|Warning \d+): |}

let doThis (err): result =
  if not (Pcre.pmatch ~rex:hasErrorOrWarningR err) then NoErrorNorWarning err
  else
    let errorContent =
      BatString.trim err
      |> Pcre.full_split ~rex:fileR
      (* First few rows might be random output info *)
      |> BatList.drop_while (function Pcre.Text _ -> true | _ -> false)
    in
    if BatList.length errorContent = 0 then Unparsable err
    else (
      let files =
        BatString.trim err
        |> Pcre.full_split ~rex:fileR
        (* First few rows might be random output info *)
        |> BatList.drop_while (function Pcre.Text _ -> true | _ -> false)
        (* we match 6 items, so the whole list will always be a multiple of 6 *)
        |> splitInto ~chunckSize:6
        |> BatList.map extractFromFileMatch
      in
      let filesAndErrorsAndWarnings: fileAndErrorsAndWarnings list =
      files |> BatList.map (fun (fileInfo, range, text) ->
        let errorsAndWarnings =
          Pcre.full_split ~rex:errorOrWarningR text
          (* we match 4 items, so the whole list will always be a multiple of 4 *)
          |> splitInto ~chunckSize:3
        in
        (* taking advantage of the distinct shape of error/warning to separate
        them *)
        let errors = errorsAndWarnings |> Pcre.(BatList.filter_map (function
          | [Delim _; NoGroup; Text text] -> Some text
          | _ -> None
        ))
        in
        let warnings = errorsAndWarnings |> Pcre.(BatList.filter_map (function
          | [Delim _; Group (_, code); Text text] ->
              Some (int_of_string code, text)
          | _ -> None
        ))
        in
        let errs =
          errors
          |> BatList.map (fun errorRaw ->
              let result =
                try
                  BatList.find_map (fun errorParser ->
                    try Some (errorParser errorRaw fileInfo range)
                    with _ -> None)
                  ErrorParsers.parsers
                with Not_found -> Error_CatchAll errorRaw
              in
              {
                range = range;
                parsedContent = result;
              }
          )
        in
        let warns =
          warnings
          |> BatList.map (fun (code, warningRaw) ->
            let result = {
              code = code;
              warningType =
                try
                  BatList.find_map (fun warningParser ->
                    try Some (warningParser code warningRaw fileInfo range)
                    with _ -> None)
                  WarningParsers.parsers
                with Not_found -> Warning_CatchAll warningRaw
            }
            in {
              range = range;
              parsedContent = result;
            }
          )
        in {fileInfo = fileInfo; errors = errs; warnings = warns}
      )
      in
      ErrorsAndWarnings filesAndErrorsAndWarnings
    )

(* entry point, for convenience purposes for now. Theoretically the parser and
the reporters are decoupled *)
let () =
  try
    let err = BatPervasives.input_all stdin in
    Reporter.print @@ doThis err;
  with BatIO.No_more_input -> ()

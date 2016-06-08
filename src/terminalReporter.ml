open BetterErrorsTypes
open Helpers

let numberOfDigits n =
  let digits = ref 1 in
  let nn = ref n in
  while !nn / 10 > 0 do
    nn := !nn / 10;
    digits := !digits + 1
  done;
  !digits

let pad ?(ch=' ') content n =
  (String.make (n - (String.length content)) ch) ^ content

let startingSpacesCount str =
  let rec startingSpacesCount' str idx =
    if idx = String.length str then idx
    else if String.get str idx <> ' ' then idx
    else startingSpacesCount' str (idx + 1)
  in startingSpacesCount' str 0

(* row and col 0-indexed; endColumn is 1 past the actual end. See
Main.compilerLineColsToRange *)
let _printFile ~highlightColor:color ~highlight:((startRow, startColumn), (endRow, endColumn)) content =
  let displayedStartRow = max 0 (startRow - 3) in
  (* we display no more than 3 lines after startRow. Some endRow are rly far
  away *)
  let displayedEndRow = min (List.length content - 1) (startRow + 3) in
  let lineNumWidth = numberOfDigits (List.length content) in
  (* sometimes the snippet of file we show is really indented. We de-indent it
  for nicer display by trimming out the maximum amount of leading spaces we can. *)
  let rowsForCountingStartingSpaces =
    listDrop displayedStartRow content
    |> listTake (displayedEndRow - displayedStartRow + 1)
    |> List.filter (fun row -> row <> "")
  in
  let minIndent =
    match rowsForCountingStartingSpaces with
    | [] -> 0
    | _ ->
      let startingSpaces = List.map startingSpacesCount rowsForCountingStartingSpaces in
      List.fold_left
        (fun acc num -> if num < acc then num else acc)
        (List.hd startingSpaces)
        startingSpaces
  in
  (* ellipsis vertical separator to indicate "there are white spaces before" *)
  let sep = if minIndent = 0 then " │ " else " ┆ " in
  let startColumn = startColumn - minIndent in
  let endColumn = endColumn - minIndent in
  let result = ref [] in
  for i = displayedStartRow to displayedEndRow do
    let currLine = List.nth content i |> stringSlice ~first:minIndent in
    if i >= startRow && i <= endRow then
      if startRow = endRow then
        result := ((pad (string_of_int (i + 1)) lineNumWidth)
          ^ sep ^ (highlight ~color ~first:startColumn ~last:endColumn currLine)) :: !result
      else if i = startRow then
        result := ((pad (string_of_int (i + 1)) lineNumWidth)
          ^ sep ^ (highlight ~color ~first:startColumn currLine)) :: !result
      else if i = endRow then
        result := ((pad (string_of_int (i + 1)) lineNumWidth)
          ^ sep ^ (highlight ~color ~last:endColumn currLine)) :: !result
      else
        result := ((pad (string_of_int (i + 1)) lineNumWidth)
          ^ sep ^ (highlight ~color currLine)) :: !result
    else
      result := ((pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ currLine) :: !result
  done;
  !result |> List.rev |> String.concat "\n"

let printFile ?(isWarning=false) {cachedContent; filePath; range} =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  let filePathDisplay = if startRow = endRow then
      cyan @@ sp
        "%s:%d %d-%d\n"
        filePath
        (startRow + 1)
        startColumn
        endColumn
    else
      cyan @@ sp
        "%s:%d:%d-%d:%d\n"
        filePath
        (startRow + 1)
        startColumn
        (endRow + 1)
        endColumn
  in filePathDisplay ^ _printFile
    ~highlightColor:(if isWarning then yellow else red)
    ~highlight:range
    cachedContent

let prettyPrintParsedResult (result: result) =
  match result with
  | Unparsable str ->
    (* output the line without any decoration around. We previously had some
    cute little ascii red x mark to say "we couldn't parse this but there's
    probably an error". But it's very possible that this line's a continuation
    of a previous error, just that we couldn't parse it. So we try to bolt this
    line right after our supposedly parsed and pretty-printed error to make them
    look like one printed error. *)
    (* the effing length we'd go for better errors... someone gimme a cookie *)
    str
  | ErrorFile NonexistentFile -> ""
  | ErrorFile (CommandLine moduleName) ->
    sp "%s: module `%s` not found." (red "Error") moduleName
  | ErrorFile (NoneFile filename) ->
    (* TODO: test case for this. Forgot how to repro it *)
    if Filename.check_suffix filename ".cmo" then
      sp
        "%s: Cannot find file %s. Cmo files are artifacts the compiler looks for when compiling/linking dependent files."
        (red "Error")
        (cyan filename)
    else sp "%s: Cannot find file %s." (red "Error") (cyan filename)
  | ErrorFile (BadFileName filepath) ->
    sp
      "%s\n\n%s 24: \"%s\" isn't a valid file name; OCaml file names are often turned into modules, which need to start with a capitalized letter."
      (cyan filepath)
      (yellow "Warning")
      (Filename.basename filepath)
  | ErrorContent withFileInfo ->
    sp "%s\n\n%s: %s" (printFile withFileInfo) (red "Error") (ReportError.report withFileInfo.parsedContent)
  | Warning withFileInfo ->
    sp
      "%s\n\n%s %d: %s"
      (printFile ~isWarning:true withFileInfo)
      (yellow "Warning")
      withFileInfo.parsedContent.code
      (ReportWarning.report withFileInfo.parsedContent.code withFileInfo.parsedContent.warningType)

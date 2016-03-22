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

(* row and col 0-indexed; endColumn is 1 past the actual end. See
Main.compilerLineColsToRange *)
let _printFile ~highlightColor:color ~highlight:((startRow, startColumn), (endRow, endColumn)) content =
  let sep = " | " in
  let displayedStartRow = max 0 (startRow - 3) in
  (* we display no more than 3 lines after startRow. Some endRow are rly far
  away *)
  let displayedEndRow = min (List.length content - 1) (startRow + 3) in
  let lineNumWidth = numberOfDigits (List.length content) in
  let result = ref "" in
  for i = displayedStartRow to displayedEndRow do
    let currLine = List.nth content i in
      if i >= startRow && i <= endRow then
        if startRow = endRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~first:startColumn ~last:endColumn currLine) ^ "\n"
        else if i = startRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~first:startColumn currLine) ^ "\n"
        else if i = endRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~last:endColumn currLine) ^ "\n"
        else
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color currLine) ^ "\n"
      else
        result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ currLine ^ "\n"
  done;
  !result

let printFile ?(isWarning=false) {cachedContent; filePath; range} =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  let filePathDisplay = if startRow = endRow then
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
  in filePathDisplay ^ _printFile
    ~highlightColor:(if isWarning then yellowUnderlined else redUnderlined)
    ~highlight:range
    cachedContent

let decryptAssumingErrorsAndWarnings = List.map (fun errorOrWarning ->
  match errorOrWarning with
  | Error {parsedContent} -> ReportError.report parsedContent
  | Warning {parsedContent={code; warningType}} -> ReportWarning.report code warningType
)

let prettyPrintParsedResult (content: result) = match content with
  (* handle the special cases first *)
  | NoErrorNorWarning content -> content ^ green "\n✔ Seems fine!"
  | Unparsable content -> content ^ red "\n✘ There might be an error."
  | ErrorsAndWarnings errorsAndWarnings ->
    String.concat "\n" @@
      List.map2 (fun errorOrWarning generatedText ->
        match errorOrWarning with
        | Error withFileInfo ->
          sp "%s\n%s: %s" (printFile withFileInfo) (red "Error") generatedText
        | Warning withFileInfo ->
          sp
            "%s\n%s %d: %s"
            (printFile ~isWarning:true withFileInfo)
            (yellow "Warning")
            withFileInfo.parsedContent.code
            generatedText
        )
      errorsAndWarnings
      (decryptAssumingErrorsAndWarnings errorsAndWarnings)

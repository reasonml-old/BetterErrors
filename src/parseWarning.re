
open BetterErrorsTypes;

open Helpers;

/* agnostic extractors, turning err string into proper data structures */
/* TODO: don't make these raise error */
let warning_UnusedVariable code err cachedContent range => raise Not_found;

/* need: what the variant is. If it's e.g. a list, instead of saying "doesn't
   cover all the cases of the variant" we could say "doesn't cover all the possible
   length of the list" */
let warning_PatternNotExhaustive code err _ _ => {
  let unmatchedR = {|this pattern-matching is not exhaustive.\sHere is an example of a value that is not matched:\s([\s\S]+)|};
  let unmatchedRaw = get_match unmatchedR err;
  let unmatched =
    if (unmatchedRaw.[0] == '(') {
      /* format was (Variant1|Variant2|Variant3). We strip the surrounding parens */
      unmatchedRaw |>
        Helpers.stringSlice first::1 last::(String.length unmatchedRaw - 1) |> split {|\|[\s]*|}
    } else {
      [unmatchedRaw]
    };
  Warning_PatternNotExhaustive {unmatched: unmatched}
};

let warning_PatternUnused code err cachedContent range => raise Not_found;

/* need: offending optional argument name from AST */
/* need: offending function name */
let warning_OptionalArgumentNotErased code err cachedContent range => {
  let ((startRow, startColumn), (endRow, endColumn)) = range;
  /* Hardcoding 16 for now. We might one day switch to use the variant from
     https://github.com/ocaml/ocaml/blob/901c67559469acc58935e1cc0ced253469a8c77a/utils/warnings.ml#L20 */
  let allR = {|this optional argument cannot be erased\.|};
  let fileLine = List.nth cachedContent startRow;
  let _ = get_match_n 0 allR err;
  let argumentNameRaw =
    Helpers.stringSlice
      first::startColumn
      last::(
        if (startRow == endRow) {
          endColumn
        } else {
          99999
        }
      )
      fileLine;
  let argumentNameR = {|(:?\?\s*\()?([^=]+)|};
  let argumentName = get_match_n 2 argumentNameR argumentNameRaw;
  Warning_OptionalArgumentNotErased {argumentName: String.trim argumentName}
};

/* TODO: better logic using these codes */
let parsers = [
  warning_UnusedVariable,
  warning_PatternNotExhaustive,
  warning_PatternUnused,
  warning_OptionalArgumentNotErased
];

let parse code warningBody cachedContent range =>
  try (
    Helpers.listFindMap
      (
        fun parse =>
          try (Some (parse code warningBody cachedContent range)) {
          | _ => None
          }
      )
      parsers
  ) {
  | Not_found => Warning_CatchAll warningBody
  };

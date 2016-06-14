
open BetterErrorsTypes;

open Helpers;

let report code filePath parsedContent =>
  switch parsedContent {
  | Warning_CatchAll message => message
  | Warning_PatternNotExhaustive {unmatched} =>
    "this match doesn't cover all possible values of the variant.\n" ^ (
      switch unmatched {
      | [oneVariant] => sp "The case `%s` is not matched" oneVariant
      | many => sp "These cases are not matched:\n%s" (mapcat "\n" (sp "- `%s`") many)
      }
    )
  | Warning_OptionalArgumentNotErased {argumentName} =>
    sp
      "`%s` is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n"
      argumentName
      argumentName
      ^ "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
  | Warning_BadFileName offendingChar =>
    sp
      "file name potentially invalid. The OCaml ecosystem's build systems usually turn file names into module names by simply upper-casing the first letter. In this case, `%s` %s.\nNote: some build systems might e.g. turn kebab-case into CamelCase module, which is why this isn't a hard error."
      /* "%s\n\n%s 24: \"%s\" isn't a valid file name; OCaml file names are often turned into modules, which need to start with a capitalized letter." */
      (Filename.basename filePath |> String.capitalize)
      (
        switch offendingChar {
        | Leading ch => sp "starts with `%s`, which doesn't form a legal module name" ch
        | Contains ch => sp "contains `%s`, which doesn't form a legal module name" ch
        | UnknownIllegalChar => "isn't a legal module name"
        }
      )
  | _ => "huh"
  };

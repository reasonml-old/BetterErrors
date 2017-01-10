open BetterErrorsTypes;

open Helpers;

let listify suggestions =>
  suggestions |> List.map (fun sug => "- `" ^ sug ^ "`") |> String.concat "\n";

let highlightPart ::color ::part str => {
  let indexOfPartInStr = Helpers.stringFind str part;
  highlight ::color first::indexOfPartInStr last::(indexOfPartInStr + String.length part) str
};

let report ::refmttypePath parsedContent => {
  let formatOutputSyntax types =>
    switch refmttypePath {
    | None => types
    | Some path =>
      let types = String.concat {|\"|} types;
      let cmd = path ^ (sp {| "%s"|}) types;
      let input = Unix.open_process_in cmd;
      let result = ref [];
      try (
        while true {
          result := [input_line input, ...!result]
        }
      ) {
      | End_of_file => ignore (Unix.close_process_in input)
      };
      List.rev !result
    };
  switch parsedContent {
  | Error_CatchAll error => error
  | Type_MismatchTypeArguments {typeConstructor, expectedCount, actualCount} =>
    sp
      "This needs to be applied to %d argument%s, we found %d."
      expectedCount
      (
        if (expectedCount == 1) {
          ""
        } else {
          "s"
        }
      )
      actualCount
  | Type_IncompatibleType {
      actual,
      expected,
      differingPortion,
      actualEquivalentType,
      expectedEquivalentType,
      extra
    } =>
    /* let (diffA, diffB) = differingPortion; */
    let (actual, expected) =
      switch (formatOutputSyntax [actual, expected]) {
      | [a, b] => (a, b)
      | _ => (actual, expected)
      };
    sp
      "The types don't match.\n%s %s\n%s  %s"
      (red "This is:")
      /* (highlightPart color::red part::diffA actual) */
      (highlight actual)
      (green "Wanted:")
      /* (highlightPart color::green part::diffB expected)  */
      (highlight color::green expected) ^ (
      switch extra {
      | Some e => "\nExtra info: " ^ e
      | None => ""
      }
    )
  | Type_NotAFunction {actual} =>
    let actual =
      switch (formatOutputSyntax [actual]) {
      | [a] => a
      | _ => actual
      };
    "This is " ^
    actual ^
    ". You seem to have called it as a function.\n" ^ "Careful with spaces, semicolons, parentheses, and whatever in-between!"
  | Type_AppliedTooMany {functionType, expectedArgCount} =>
    let functionType =
      switch (formatOutputSyntax [functionType]) {
      | [a] => a
      | _ => functionType
      };
    sp
      "This function has type %s\nIt accepts only %d arguments. You gave more. Maybe you forgot a `;` somewhere?"
      functionType
      expectedArgCount
  | File_SyntaxError {offendingString, hint} =>
    (
      switch hint {
      | Some a => "The syntax is wrong: " ^ a
      | None => "The syntax is wrong."
      }
    ) ^
    "\n" ^
    (
      switch offendingString {
      | ";" => "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\").\n"
      | "else" =>
        "Did you happen to have put a semicolon on the line before else?" ^ " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses.\n"
      | _ => ""
      }
    ) ^ "Note: the location indicated might not be accurate."
  | File_IllegalCharacter {character} =>
    sp "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!" character
  | Type_UnboundTypeConstructor {namespacedConstructor, suggestion} =>
    let namespacedConstructor =
      switch (formatOutputSyntax [namespacedConstructor]) {
      | [a] => a
      | _ => namespacedConstructor
      };
    sp "The type constructor %s can't be found." namespacedConstructor ^ (
      switch suggestion {
      | None => ""
      | Some h => sp "\nHint: did you mean `%s`?" h
      }
    )
  | Type_UnboundValue {unboundValue, suggestions} =>
    switch suggestions {
    | None => sp "`%s` can't be found. Could it be a typo?" unboundValue
    | Some [hint] => sp "`%s` can't be found. Did you mean `%s`?" unboundValue hint
    | Some [hint1, hint2] =>
      sp "`%s` can't be found. Did you mean `%s` or `%s`?" unboundValue hint1 hint2
    | Some hints =>
      sp "`%s` can't be found. Did you mean one of these?\n%s" unboundValue (listify hints)
    }
  | Type_UnboundRecordField {recordField, suggestion} =>
    let recordField =
      switch (formatOutputSyntax [recordField]) {
      | [a] => a
      | _ => recordField
      };
    switch suggestion {
    | None => sp "Field `%s` can't be found in any record type." recordField
    | Some hint =>
      sp "Field `%s` can't be found in any record type. Did you mean `%s`?" recordField hint
    }
  | Type_UnboundModule {unboundModule, suggestion} =>
    let unboundModule =
      switch (formatOutputSyntax [unboundModule]) {
      | [a] => a
      | _ => unboundModule
      };
    sp "Module `%s` not found in included libraries.\n" unboundModule ^ (
      switch suggestion {
      | Some s => sp "Hint: did you mean `%s`?" s
      | None =>
        let pckName = String.lowercase unboundModule;
        "Hint: your build rules might be missing a link. If you're using: \n" ^
        " - Oasis: make sure you have `" ^
        pckName ^
        "` under `BuildDepends` in your _oasis file.\n" ^
        " - ocamlbuild: make sure you have `-pkgs " ^
        pckName ^
        "` in your build command.\n" ^
        " - ocamlc | ocamlopt: make sure you have `-I +" ^
        pckName ^
        "` in your build command before the source files.\n" ^
        " - ocamlfind: make sure you have `-package " ^
        pckName ^ " -linkpkg` in your build command."
      }
    )
  | _ => "huh"
  }
};

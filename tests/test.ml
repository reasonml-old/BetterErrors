(* Note: this file must be run at root directory of the project. Otherwise the
Sys.command calls below happen in the wrong directory *)

(* File's in ocaml for now, because our `ocaml tests/test.ml` call doesn't allow
us to pass the `-pp refmt` option to parse reason code. Would be nice if `ocaml`
had that like `ocamlc` *)

let folders = [
  (* (directory, number of tests) *)
  (* first one is special. See the actual tests loop below *)
  ("specialTests", 4);
  ("noError", 1);
  ("prettyPrint", 2);
  ("1_bad_file_name", 1);
  ("bad-file-name-2", 1);
  ("file_IllegalCharacter", 1);
  ("file_SyntaxError", 6);
  ("type_AppliedTooMany", 2);
  ("type_AppliedWithoutLabel", 1);
  ("type_IncompatibleType", 7);
  ("type_MismatchTypeArguments", 1);
  ("type_NotAFunction", 1);
  ("type_RecordFieldNotBelong", 2);
  ("type_RecordFieldsUndefined", 1);
  ("type_UnboundModule", 2);
  ("type_UnboundRecordField", 2);
  ("type_UnboundTypeConstructor", 2);
  ("type_UnboundValue", 4);
  ("warning_OptionalArgumentNotErased", 2);
  ("warning_PatternNotExhaustive", 2);
  ("warning_PatternUnused", 1);
]

exception Not_equal of string

let readFile filePath =
  let lines = ref [] in
  let chan = open_in filePath in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    "this will never be reached"
  with End_of_file ->
    close_in chan;
    List.rev !lines |> String.concat "\n"

(* these generate ocaml errors that points to nonexistant files. Handle them
specially here *)
let specialTestsCommands = [
  "ocamlc -open NonExistantModule foo.ml";
  "ocamlc nonexistentFile.cmo";
  "ocamlc nonexistentFile.ml";
  "echo \"let a:string = 1\" | utop -stdin"
]

let () =
  try
    folders
    |> List.iteri (fun i (dirname, fileCount) -> for j = 1 to fileCount do
      let testsDirname = Filename.concat "tests" dirname in
      let filename = Filename.concat testsDirname (Printf.sprintf "%s_%d.ml" dirname j) in
      let expectedOutputName = Filename.concat testsDirname (Printf.sprintf "%s_%d_expected.txt" dirname j) in
      let actualOutputName = Filename.concat testsDirname (Printf.sprintf "%s_%d_actual.txt" dirname j) in
      (* special handling of the first item, specialTests *)
      let cmd = if i = 0 then List.nth specialTestsCommands (j - 1) else "ocamlc " ^ filename in
      (* expecting compiling errors in stderr; pipe to a file *)
      ignore @@ Sys.command @@ Printf.sprintf "%s 2>&1 | ./_build/top/app.out > %s" cmd actualOutputName;
      (* open the produced error output *)
      let expected = readFile expectedOutputName in
      let actual = readFile actualOutputName in
      (* swap-comment below two lines if you want to generate new expected
      from the new actual *)

      (* ignore @@ Sys.command @@ Printf.sprintf "cp %s %s" actualOutputName expectedOutputName *)
      (* TODO: show the differences *)
      if actual <> expected then (
        print_endline "Actual:";
        print_endline actual;
        print_endline "Expected:";
        print_endline expected;
        raise (Not_equal filename)
      )
    done);
    print_endline "ALL GOOD!";
    ignore @@ Sys.command "rm -rf ./tests/**/*.{cmi,cmo}";
  (* trust me I'm not evil *)
  (* the leftover cmi and cmo files from some partially failed ocamlc above
  cause the next `make` build to fail out of refusal to compile with these
  leftover artifact, so we remove them *)
  with a ->
    ignore @@ Sys.command "rm -rf ./tests/**/*.{cmi,cmo}";
    raise a

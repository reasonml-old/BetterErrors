(* let folders = [
  (* [directory, files] *)
  ["bad source file name"; "bad source file name.ml"];
  ["file_IllegalCharacter"; "file_IllegalCharacter.ml"];
  ["file_SyntaxError"; "file_SyntaxError_1.ml"; "file_SyntaxError_2.ml"; "file_SyntaxError_3.ml"];
  ["file_SyntaxError"; "file_SyntaxError.ml"];
  ["type_AppliedTooMany"; "type_AppliedTooMany.ml"];
  ["type_AppliedWithoutLabel"; "type_AppliedWithoutLabel.ml"];
  ["type_IncompatibleType"; "type_IncompatibleType_1.ml", "type_IncompatibleType_2.ml"];
  ["type_NotAFunction"; "type_NotAFunction.ml"];
  ["type_RecordFieldNotBelong"; "type_RecordFieldNotBelong_1.ml", "type_RecordFieldNotBelong_2.ml"];
  ["type_RecordFieldsUndefined"; "type_RecordFieldsUndefined.ml"];
  ["type_UnboundRecordField"; "type_UnboundRecordField_1.ml", "type_UnboundRecordField_2.ml"];
  ["type_UnboundTypeConstructor"; "type_UnboundTypeConstructor.ml"];
  ["warning_OptionalArgumentNotErased"; "warning_OptionalArgumentNotErased.ml"];
]; *)

(* Note: this file must be run at root directory of the project. Otherwise the
Sys.command calls below happen in the wrong directory *)

let folders = [
  (* (directory, number of tests) *)
  ("1_bad_file_name", 1);
  ("file_IllegalCharacter", 1);
  ("file_SyntaxError", 3);
  ("type_AppliedTooMany", 1);
  ("type_AppliedWithoutLabel", 1);
  ("type_IncompatibleType", 2);
  ("type_NotAFunction", 1);
  ("type_RecordFieldNotBelong", 2);
  ("type_RecordFieldsUndefined", 1);
  ("type_UnboundRecordField", 2);
  ("type_UnboundTypeConstructor", 1);
  ("warning_OptionalArgumentNotErased", 1);
]

let () =
  List.iter (fun (dirname, fileCount) -> for i = 1 to fileCount do
    let testsDirname = Filename.concat "tests" dirname in
    let filename = Filename.concat testsDirname (Printf.sprintf "%s_%d.ml" dirname i) in
    let expectedOutputName = Filename.concat testsDirname (Printf.sprintf "%s_%d_expected.txt" dirname i) in
    let actualOutputName = Filename.concat testsDirname (Printf.sprintf "%s_%d_actual.txt" dirname i) in
      (* expecting compiling errors in stderr; pipe to a file *)
      ignore @@ Sys.command @@ Printf.sprintf "ocamlc %s 2> %s" filename actualOutputName;
      (* open the produced error output *)
      BatFile.with_file_in expectedOutputName (fun inp ->
        let expected = BatIO.read_all inp in
        BatFile.with_file_in actualOutputName (fun inp2 ->
          let actual = BatIO.read_all inp2 in
          assert (expected = actual)
        )
      )

  done) folders;
  print_endline "ALL GOOD!"

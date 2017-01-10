let usage = {|BetterErrors

[Usage]:
myBuildOutput 2>&1 | berror

Output errors in Reason syntax:
myBuildOutput 2>&1 | berror --path-to-refmttype <refmttype binary here>
|};

let refmttypePath = ref None;

let options = [
  (
    "--path-to-refmttype",
    Arg.String (fun x => refmttypePath := Some x),
    "<parse>, parse AST as <parse> (either 'ml', 're', 'binary_reason(for interchange between Reason versions)', 'binary (from the ocaml compiler)')"
  )
];

let () =
  Arg.parse
    options
    (
      fun arg =>
        prerr_endline "BetterErrors (berror) doesn't accept anonymous arguments in the command line."
    )
    usage;

1;

Index.parseFromStdin refmttypePath::!refmttypePath customErrorParsers::[];

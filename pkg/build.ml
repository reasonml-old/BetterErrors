#!/usr/bin/env ocaml

#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "BetterErrors" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "src/helpers";
    Pkg.bin ~auto:true "src/betterErrorsShell" ~dst:"betterErrorsShell";
    Pkg.lib ~exts:Exts.library "src/BetterErrors";
    Pkg.doc "README.md";
  ]

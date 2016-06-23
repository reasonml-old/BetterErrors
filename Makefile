# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

install:
	opam pin add BetterErrors . -y

BETTERERRORS_ARTIFACTS       := _build/src/Atom.cmi _build/src/Atom.cmo _build/src/Atom.cmx _build/src/Atom.ml _build/src/Atom.o _build/src/BetterErrors.a _build/src/BetterErrors.cma _build/src/BetterErrors.cmxa _build/src/BetterErrors.cmxs* _build/src/BetterErrors.mllib _build/src/NuclideReporter.cmi _build/src/NuclideReporter.cmo _build/src/NuclideReporter.cmx _build/src/NuclideReporter.ml _build/src/NuclideReporter.o _build/src/betterErrorsMain.cmi _build/src/betterErrorsMain.cmo _build/src/betterErrorsMain.cmx _build/src/betterErrorsMain.ml _build/src/betterErrorsMain.o _build/src/betterErrorsParseError.cmi _build/src/betterErrorsParseError.cmo _build/src/betterErrorsParseError.cmx _build/src/betterErrorsParseError.ml _build/src/betterErrorsParseError.o _build/src/betterErrorsShell.cmi _build/src/betterErrorsShell.cmo _build/src/betterErrorsShell.cmx _build/src/betterErrorsShell.ml _build/src/betterErrorsShell.native* _build/src/betterErrorsShell.o _build/src/betterErrorsTypes.cmi _build/src/betterErrorsTypes.cmo _build/src/betterErrorsTypes.cmx _build/src/betterErrorsTypes.ml _build/src/betterErrorsTypes.o _build/src/helpers.cmi _build/src/helpers.cmo _build/src/helpers.cmx _build/src/helpers.ml _build/src/helpers.o _build/src/parseWarning.cmi _build/src/parseWarning.cmo _build/src/parseWarning.cmx _build/src/parseWarning.ml _build/src/parseWarning.o _build/src/reportError.cmi _build/src/reportError.cmo _build/src/reportError.cmx _build/src/reportError.ml _build/src/reportError.o _build/src/reportWarning.cmi _build/src/reportWarning.cmo _build/src/reportWarning.cmx _build/src/reportWarning.ml _build/src/reportWarning.o _build/src/terminalReporter.cmi _build/src/terminalReporter.cmo _build/src/terminalReporter.cmx _build/src/terminalReporter.ml _build/src/terminalReporter.o

install-via-ocamlfind:
	ocamlfind install BetterErrors pkg/META $(BETTERERRORS_ARTIFACTS)

clean:
	ocamlbuild -clean

.PHONY: build clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

.PHONY: release

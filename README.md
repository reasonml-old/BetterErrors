#### Before
![Before](https://cloud.githubusercontent.com/assets/1909539/12805314/560a9a6c-cac8-11e5-89fd-aad7d1a3f70a.png)

#### After
![After](https://cloud.githubusercontent.com/assets/1909539/12805315/561ef3fe-cac8-11e5-8ea6-cc916529d87e.png)

**Work in progress!**

If you feel adventurous, `git clone` this repo, install the modules necessary through opam (Pcre, Batteries, ANSITerminal), then run:

```sh
oasis setup -setup-update dynamic
make
```

It'll generate a `main.byte`. Its job is to take from stdin some ocaml error message, and pipe it out again, pristine and happy.

The typical workflow command would be:

```sh
ocamlc myApp.ml |& ./main.byte
```

Where the `|&` is a bash shortcut for `2>&1 |`, which in turn means "pipe the stuff from stderr into stdout, then pipe it back into stdin of the next command (aka main.bytes)".

Have fun!

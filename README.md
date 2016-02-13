#### Before
![Before](https://cloud.githubusercontent.com/assets/1909539/13025465/4baf80c2-d1d6-11e5-8f88-1d7b8065567c.png)

#### After
![Glorious After](https://cloud.githubusercontent.com/assets/1909539/13025466/4bc78262-d1d6-11e5-9dcc-2f9046dc1950.png)

#### Before
![Before](https://cloud.githubusercontent.com/assets/1909539/13025491/a47377f4-d1d6-11e5-9c12-c0b5285dba47.png)

#### After
![Glorious After](https://cloud.githubusercontent.com/assets/1909539/13025492/a4895d30-d1d6-11e5-996a-b7e0e2ba63bf.png)

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

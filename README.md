#### Before
![Before](https://cloud.githubusercontent.com/assets/1909539/13025465/4baf80c2-d1d6-11e5-8f88-1d7b8065567c.png)

#### After
![Glorious After](https://cloud.githubusercontent.com/assets/1909539/13025466/4bc78262-d1d6-11e5-9dcc-2f9046dc1950.png)

#### Before
![Before](https://cloud.githubusercontent.com/assets/1909539/13025491/a47377f4-d1d6-11e5-9c12-c0b5285dba47.png)

#### After
![Glorious After](https://cloud.githubusercontent.com/assets/1909539/13025492/a4895d30-d1d6-11e5-996a-b7e0e2ba63bf.png)

**Work in progress!**

```
npm install -g ocamlBetterErrors
```

This'll expose a global `berror` CLI util, for you to use like so:
```sh
ocamlc myApp.ml |& berror
```

**Explanation**: `|&` is a bash shortcut for `2>&1 |` (not available in vanilla sh), which, in turn, means "pipe the stuff from stderr into stdout, then pipe it back into stdin of the next command". `berror` takes in this info and searches for errors to pretty-print back.

Have fun!

### For Development
`git clone` this repo, `cd` into it, then run:

```sh
npm install
# to compile
npm start
# to test
npm test
```

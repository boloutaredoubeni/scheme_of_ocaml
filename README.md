## A lisp interpreter

### Depencencies

1. OCaml
2. Opam

```bash
opam install core
opam install alcotest # for testing of course
```

### Run it
```bash
./corebuild src/main.native
```

#### Set up Utop
```bash
ocamlfind ocamlmktop -o proj_utop -thread -package utop proj_utop.cmo
```

### Run the tests
```bash
./corebuild src/lisp_test.native -pkg alcotest && ./lisp_test.native
```

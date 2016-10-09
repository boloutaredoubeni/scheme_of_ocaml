### Run it
```
corebuild src/lisp.{byte,native} -pkg ollvm
```

#### Set up Utop
```
ocamlfind ocamlmktop -o proj_utop -thread -package utop proj_utop.cmo
```
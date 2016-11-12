open Core.Std

val parse_sexp: ('a Lisp.expr, Sexp.t) Result.t -> ('b Lisp.expr, Sexp.t) Result.t 

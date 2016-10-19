open Core.Std

val add: key:(Lisp.t) -> data:(Lisp.t) -> unit

val lookup: Lisp.t -> Lisp.t Option.t

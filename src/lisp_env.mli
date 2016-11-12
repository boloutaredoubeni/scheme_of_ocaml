open Core.Std

open Lisp

val add: key:('a primitive expr) -> data:('b primitive expr) -> unit

val lookup: ('a primitive expr) -> 'b primitive expr Option.t

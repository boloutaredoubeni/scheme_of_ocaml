open Lisp
open Core.Std
open Lisp_env
open Common

let rec eval : type lisp. lisp expr -> lisp  = function
  | Lisp_value (Lisp_bool b) -> b
  | Lisp_value (Lisp_atom atom) -> atom (** TODO: look up in env*)
  | Lisp_value (Lisp_string str) ->str
  | Lisp_value (Lisp_number num) -> num
  | Lisp_value (Lisp_list lst) -> lst
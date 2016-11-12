
(** Lisp types *)
type _ primitive =
  | Lisp_atom : string -> string primitive
  (* (string <atom>) *)
  | Lisp_string : string -> string primitive
  (* true false *)
  | Lisp_bool : bool -> bool primitive
  (* 1 2 .. 10.0*)
  | Lisp_number : float -> float primitive
  (* (x1 x2 ... xn) *)
  | Lisp_list :  'a list -> 'a list primitive 
  (* (lambda (arg1 .. argn) <expr>) *)
  | Lisp_lambda : (string primitive * string primitive list * 'a primitive) -> (string * string list * 'a) primitive

type _ expr =
  | Lisp_value : 'a primitive -> 'a expr
  | Lisp_eq : 'a expr list -> bool expr
  | Lisp_neq : 'a expr list -> bool expr
  | Lisp_cond : (bool expr * 'a expr) list expr
  | Lisp_first : 'a expr  -> 'a expr
  | Lisp_rest : 'a expr -> 'a expr
  | Lisp_quote : 'a expr -> 'a expr
  | Lisp_add : float list expr -> float expr
  | Lisp_sub : float list expr -> float expr
  | Lisp_div : float list expr -> float expr
  | Lisp_mul : float list expr -> float expr
  | Lisp_lt : float list expr -> bool expr
  | Lisp_gt : float list expr -> bool expr
  | Lisp_lte : float list expr -> bool expr
  | Lisp_gte : float list expr -> bool expr
  | Lisp_and : bool list expr -> bool expr
  | Lisp_or : bool list expr -> bool expr
  (* | Not : t *)
  | Lisp_cons : 'a expr * 'b expr -> ('a * 'b) expr
  | Lisp_def : (string expr * string expr list * 'a expr) -> (string * string list * 'a) expr

let show = fun expr -> "TODO: print something"

let equal a b = false
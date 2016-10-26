open Lisp
open Core.Std
open Lisp_env
open Common


let rec get_number = function
  | Number n  -> Or_error.return n
  | List [n] -> get_number n
  | _ -> failwith "Error type mismatch"

let rec id = function
  | List [v] -> id v
  | any -> Or_error.return any

let num_binop op a b =
  match a, b with
  | Number a, Number b -> Number (op a b)
  | _ -> failwith "Type mismatch"

let not_equal a b = not @@ Lisp.equal a b

let lt a b =
  match a, b with
  | Number n, Number m -> n < m
  | String s, String t -> s < t
  | List xs, List ys -> xs < ys
  | _ -> failwith "Cannot compare these values"

let gt a b =
  match a, b with
  | Number n, Number m -> n > m
  | String s, String t -> s > t
  | List xs, List ys -> xs > ys
  | _ -> failwith "Cannot compare these values"

let gte a b =
  match a, b with
  | Number n, Number m -> n >= m
  | String s, String t -> s >= t
  | List xs, List ys -> xs >= ys
  | _ -> failwith "Cannot compare these values"

let lte a b =
  match a, b with
  | Number n, Number m -> n <= m
  | String s, String t -> s <= t
  | List xs, List ys -> xs <= ys
  | _ -> failwith "Cannot compare these values"

let lisp_and a b =
  match (a, b) with
  | Lisp.Bool n, Lisp.Bool m -> Bool (n && m)
  | Number n, Number m ->
    let l = int_of_float n in
    let r = int_of_float m in
    Number (float_of_int @@ l land r)
  | _ -> failwith "Cannot compare these values"

let lisp_or a b =
  match (a, b) with
  | Lisp.Bool n, Lisp.Bool m -> Bool (n || m)
  | Number n, Number m ->
    let l = int_of_float n in
    let r = int_of_float m in
    Number (float_of_int @@ l lor r)
  | _ -> failwith "Cannot compare these values"


let bool_binop op a b = Bool (op a b)

let car = function
  | Lisp.List (x::_) -> Or_error.return x
  | _ -> failwith "Bad Arguement"

let cdr = function
  | Lisp.List (_::xs) -> Or_error.return @@ List xs
  | _ -> failwith "Bad Arguement"

let cons a b =
  match a, b with
  | _, (Lisp.List []) -> List [a]
  | _, (Lisp.List xs) -> List (a::xs)
  | a, b -> List [a; b]


let apply fn args =
  args
  |> List.reduce_exn ~f:fn
  |> Or_error.return

let rec evaluate lisp =
  let open Or_error.Monad_infix in
  match lisp with
  | String _ | Bool _ | Number _ -> Or_error.return lisp
  | Atom _ as atom ->
    begin
      match lookup atom with
      | Some def -> Or_error.return def
      | None -> Or_error.return atom
    end
  | Quote lisp -> Or_error.return lisp
  | Add atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (num_binop (+.)))
  | Sub atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (num_binop (-.)))
  | Div atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (num_binop (/.)))
  | Mul atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (num_binop ( *.)))
  | Eq atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop Lisp.equal))
  | Neq atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop not_equal))
  | Lt atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop lt))
  | Lte atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop lte))
  | Gt atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop gt))
  | Gte atoms -> (List.traverse ~f:evaluate atoms) >>= (apply (bool_binop gte))
  | And atoms -> (List.traverse ~f:evaluate atoms) >>= (apply lisp_and)
  | Or atoms -> (List.traverse ~f:evaluate atoms) >>= (apply lisp_or)
  | First atom -> print_endline @@ "Whos on first again" ^ Lisp.show atom; (evaluate atom) >>= car
  | Rest atom -> (evaluate atom) >>= cdr
  | Cons (x, y) -> [evaluate x; evaluate y] |> Or_error.all >>= (apply cons)
  | Lambda (params, body) as fn ->
    begin
      let name = List.hd_exn params in
      let args = List.tl params in
      match args with
      | None ->
        Lisp_env.add ~key:name ~data:body;
        Or_error.return @@ Lisp.List []
      | Some _ ->
        Lisp_env.add ~key:name ~data:fn;
        Or_error.return @@ Lisp.List []
    end
  | Cond atoms ->
    (* TODO: dont't allow these to mutate state *)
    let (conds, exprs) = List.unzip atoms in
    let rest_of_conds = List.drop_while conds ~f:(fun cond ->
        match evaluate cond with
        | Result.Ok Lisp.Bool false -> true
        | Result.Ok _ -> false
        | _ -> false
      ) in
    let idx = (List.length rest_of_conds) - 1 in
    begin
      match List.nth exprs idx with
        Some lisp -> evaluate lisp
      | None -> failwith "Invalid conds"
    end
  | List atoms -> (List.traverse ~f:evaluate atoms) >>= apply_fn
and apply_fn exprs =
  let fn = List.hd_exn exprs in
  let number_of_args = exprs
                       |> List.tl
                       |> Option.value_map
                         ~f:(fun lst -> List.length lst)
                         ~default:0
  in
  let (params, fn_body) =
    match fn with
    | Lisp.Lambda (params, body) -> (List.tl_exn params, body)
    | _ -> failwith "Logic Error"
  in
  let number_of_params = params |> List.length in
  if number_of_args <> number_of_params then
    raise @@ Failure ((string_of_int number_of_params) ^ " " ^ (string_of_int @@ number_of_args))
  else
    let _exprs = exprs |> List.tl_exn in
    let _ = List.iter2_exn _exprs params ~f:(fun arg param_name ->
      Lisp_env.add ~key:param_name ~data:arg
      ) in
    evaluate fn_body

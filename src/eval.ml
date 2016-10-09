open Lisp
open Core.Std
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


let apply fn (args: t list) =
    args
    |> List.reduce_exn ~f:fn
    |> Or_error.return

let rec evaluate lisp =
    let open Or_error.Monad_infix in
    match lisp with 
    | Atom _ | String _ | Bool _ | Number _ -> Or_error.return lisp
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
    | First atom -> (evaluate atom) >>= car
    | Rest atom -> (evaluate atom) >>= cdr
    | Cons (x, y) -> [evaluate x; evaluate y] |> Or_error.all >>= (apply cons)
    | Cond atoms -> 
        (* TODO: dont't allow these to mutate state *)
        print_endline @@ "0 " ^Lisp.show lisp;
        let (conds, exprs) = List.unzip atoms in
        print_endline @@ "0.1 " ^ Lisp.show @@ List conds;
        print_endline @@ "0.2 " ^ Lisp.show @@ List exprs;
        let rest_of_conds = List.drop_while conds ~f:(fun cond -> 
     
            match evaluate cond with 
            | Ok Lisp.Bool false ->        print_endline @@ "1.1 " ^Lisp.show cond;true
            | Ok l -> print_endline @@ "1.2 " ^Lisp.show l; false
            | _ -> false
        ) in
        print_endline @@ "1.3 " ^ Lisp.show @@ List rest_of_conds;
       let idx = (List.length rest_of_conds) - 1 in
       begin
        match List.nth exprs idx with 
        Some lisp -> evaluate lisp
        | None -> failwith "Invalid conds"
        end
        (*Option.value_map switch_point
            ~f:(fun switch -> 
                (*let idx = List.length rest_of_conds in*)
                print_endline @@ "2 " ^Lisp.show switch;
                match evaluate switch with 
                | Ok Bool true -> Or_error.return @@ Bool true
                | _ -> Or_error.return @@ List []
            )
            ~default:(failwith "Not a cond expr")*)
    | err -> failwith @@ Lisp.show err

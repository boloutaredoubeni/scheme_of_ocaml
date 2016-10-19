open Core.Std
open Lisp
open Common
open Result.Let_syntax

let choose parser tokens =
  parser
  |> List.fold
    ~f:(fun tokens parse_fn -> parse_fn tokens)
    ~init:tokens

let parse_atom = function
  | Ok lisp -> Result.return lisp
  | Error (Sexp.Atom atom) -> Result.return @@ Atom atom
  | err -> err

let parse_bool = function
  | Ok lisp -> Result.return lisp
  | Error sexp as err->
    try
      Result.return @@ Bool (bool_of_sexp @@ sexp)
    with
    | _ -> err

let parse_number = function
  | Ok lisp -> Result.return lisp
  | Error sexp as err->
    try
      Result.return @@ Number (float_of_sexp @@ sexp)
    with
    | _ -> err

let rec parse_sexp sexp =
  sexp
  |> choose
    [
      parse_number;
      parse_bool;
      parse_atom;
      parse_function;
      parse_neq;
      parse_eq;
      parse_first_expr;
      parse_quote;
      parse_rest_expr;
      parse_add;
      parse_and;
      parse_or;
      parse_div;
      parse_sub;
      parse_mult;
      parse_cons;
      parse_cond;
      parse_list;
    ]
and parse_list = function
  | Ok lisp -> Result.return lisp
  | Error (Sexp.List lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ List l
      | _ -> err
    end
  | err -> err
and parse_function = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom kwd :: params :: body) when kwd = "define" || kwd = "lambda" ->
    let args = parse_params @@ parse_sexp @@ Error params in
    let fn_body = match body with
      | [expr] -> begin
          match Result.ok @@ parse_sexp @@ Error expr with
          | Some body_of_function -> body_of_function
          | None -> failwith "Improper function body"
        end
      | _ -> failwith "Improper function body" in
    Result.return @@ Lambda (args, fn_body)
  | err -> err
and parse_params = function
  | Ok (Atom _ as param) -> [param]
  | Ok List params when List.for_all params ~f:(fun p -> match p with Atom _ -> true | _ -> false) ->
    params
  | _ -> failwith "Failed to parse function argument"
and parse_eq = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "=":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Eq l
      | _ -> err
    end
  | err -> err
and parse_neq = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "/=":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Neq l
      | _ -> err
    end
  | err -> err
and parse_first_expr = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "first":: rest) ->
    begin
      match parse_sexp @@ Error (Sexp.List rest) with
      | Ok List l -> Result.return @@ First (List l)
      | Ok l -> Result.return @@ First l
      | _ ->  failwith "Invalid use of first"
    end
  | err -> err
and parse_rest_expr = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "rest":: rest) ->
    begin
      match parse_sexp @@ Error (Sexp.List rest) with
      | Ok l -> Result.return @@ Rest l
      | _ ->  failwith "Invalid use of rest"
    end
  | err -> err
and parse_quote = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "quote":: rest) ->
    begin
      match parse_sexp @@ Error (Sexp.List rest) with
      | Ok l -> Result.return @@ Quote l
      | _ ->  failwith "Invalid use of quote"
    end
  | err -> err
and parse_add = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "+" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Add l
      | _ -> err
    end
  | err -> err
and parse_sub = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "-" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Sub l
      | _ -> err
    end
  | err -> err
and parse_div = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "/" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Div l
      | _ -> err
    end
  | err -> err
and parse_mult = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "*" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Mul l
      | _ -> err
    end
  | err -> err
and parse_and = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "&&":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ And l
      | _ -> err
    end
  | err -> err
and parse_or = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "||":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)
      with
      | Ok l -> Result.return @@ Or l
      | _ -> err
    end
  | err -> err
and parse_cons = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List ([Sexp.Atom "cons"; l; r]) as err ->
    begin
      let left = parse_sexp @@ Error l in
      let right = parse_sexp @@ Error r in
      match (left, right) with
      | Ok l, Ok r -> Result.return @@ Cons (l, r)
      | _ -> err
    end
  | err -> err
and parse_cond = function
  | Ok lisp -> Result.return lisp
  | Error Sexp.List (Sexp.Atom "cond":: rest) as err->
    begin
      let parsed_vals = rest
                        |> List.traverse ~f:(fun s -> parse_sexp @@ Error s)  in
      match
        parsed_vals
        |> Result.map ~f:(fun l ->
            List.map l ~f:(fun elem ->
                match elem with
                | List [x;y] -> (x, y)
                | _ -> failwith "Not a pair"
              )
          )
      with
      | Ok cond -> Result.return @@ Cond cond
      | _ -> err
    end
  | err -> err

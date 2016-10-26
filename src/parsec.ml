open Core.Std
open Common
open Result.Let_syntax

let choose parser tokens =
  parser
  |> List.fold
    ~f:(fun tokens parse_fn -> parse_fn tokens)
    ~init:tokens

let parse_atom = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error (Sexp.Atom atom) -> Result.return @@ Lisp.Atom atom
  | err -> err

let parse_string = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error (Sexp.Atom atom) as err ->
    if (String.get atom 0) = '\'' then
      err
    else
      Result.return @@ Lisp.String atom
  | err -> err

let parse_bool = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error sexp as err->
    try
      Result.return @@ Lisp.Bool (bool_of_sexp @@ sexp)
    with
    | _ -> err

let parse_number = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error sexp as err->
    try
      Result.return @@ Lisp.Number (float_of_sexp @@ sexp)
    with
    | _ -> err

let rec parse_sexp sexp =
  sexp
  |> choose
    [
      parse_number;
      parse_bool;
      parse_string;
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
  | Result.Ok lisp -> Result.return lisp
  | Result.Error (Sexp.List lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.List l
      | _ -> err
    end
  | err -> err
and parse_function = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom kwd :: params :: body) when kwd = "define" || kwd = "lambda" ->
    let args = parse_params @@ parse_sexp @@ Result.Error params in
    let fn_body = match body with
      | [expr] -> begin
          match Result.ok @@ parse_sexp @@ Result.Error expr with
          | Some body_of_function -> body_of_function
          | None -> failwith "Improper function body"
        end
      | _ -> failwith "Improper function body" in
    Result.return @@ Lisp.Lambda (args, fn_body)
  | err -> err
and parse_params = function
  | Result.Ok (Lisp.Atom _ as param) -> [param]
  | Result.Ok Lisp.List params when List.for_all params ~f:(fun p -> match p with Lisp.Atom _ -> true | _ -> false) ->
    params
  | _ -> failwith "Failed to parse function argument"
and parse_eq = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "=":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Eq l
      | _ -> err
    end
  | err -> err
and parse_neq = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "/=":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Neq l
      | _ -> err
    end
  | err -> err
and parse_first_expr = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "first":: rest) ->
    begin
      match parse_sexp @@ Result.Error (Sexp.List rest) with
      | Result.Ok Lisp.List l -> if (List.length l) > 0 then 
        Result.return @@ Lisp.First (Lisp.List l)
        else failwith "`first' called with no args"
      | Result.Ok l -> Result.return @@ Lisp.First l
      | _ ->  failwith "Invalid use of first"
    end
  | err -> err
and parse_rest_expr = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "rest":: rest) ->
    begin
      match parse_sexp @@ Result.Error (Sexp.List rest) with
      | Result.Ok l -> Result.return @@ Lisp.Rest l
      | _ ->  failwith "Invalid use of rest"
    end
  | err -> err
and parse_quote = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "quote":: rest) ->
    begin
      match parse_sexp @@ Result.Error (Sexp.List rest) with
      | Result.Ok l -> Result.return @@ Lisp.Quote l
      | _ ->  failwith "Invalid use of quote"
    end
  | err -> err
and parse_add = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "+" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Add l
      | _ -> err
    end
  | err -> err
and parse_sub = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "-" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Sub l
      | _ -> err
    end
  | err -> err
and parse_div = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "/" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Div l
      | _ -> err
    end
  | err -> err
and parse_mult = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "*" :: lst) as err ->
    begin
      match
        lst
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Mul l
      | _ -> err
    end
  | err -> err
and parse_and = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "&&":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.And l
      | _ -> err
    end
  | err -> err
and parse_or = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "||":: rest) as err ->
    begin
      match
        rest
        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)
      with
      | Result.Ok l -> Result.return @@ Lisp.Or l
      | _ -> err
    end
  | err -> err
and parse_cons = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List ([Sexp.Atom "cons"; l; r]) as err ->
    begin
      let left = parse_sexp @@ Result.Error l in
      let right = parse_sexp @@ Result.Error r in
      match (left, right) with
      | Result.Ok l, Result.Ok r -> Result.return @@ Lisp.Cons (l, r)
      | _ -> err
    end
  | err -> err
and parse_cond = function
  | Result.Ok lisp -> Result.return lisp
  | Result.Error Sexp.List (Sexp.Atom "cond":: rest) as err->
    begin
      let parsed_vals = rest
                        |> List.traverse ~f:(fun s -> parse_sexp @@ Result.Error s)  in
      match
        parsed_vals
        |> Result.map ~f:(fun l ->
            List.map l ~f:(fun elem ->
                match elem with
                | Lisp.List [x;y] -> (x, y)
                | _ -> failwith "Not a pair"
              )
          )
      with
      | Result.Ok cond -> Result.return @@ Lisp.Cond cond
      | _ -> err
    end
  | err -> err

open Core.Std
open Parsec
open Eval


let read_sexp input =
  try
    Result.Error (Sexp.of_string @@ input)
  with
  | _ -> failwith "Failed to parse input"


let print_expr = function
  | Result.Ok lisp -> String.concat ~sep:" " [";;=>"; Lisp.show lisp; "\n"]
  | _ -> failwith "Error"

let rec repl () =
  let printer to_print =
    Printf.sprintf "%s" @@ to_print
    |> Out_channel.output_string stdout;
    Out_channel.flush stdout;
  in
  printer "λ: ";
  match In_channel.input_line stdin with
    None
  | Some "" -> repl ()
  | Some "quit" -> ()
  | Some expr ->
    try
      let u = expr
              |> read_sexp
              |> parse_sexp
              |> Result.ok
              |> Option.map ~f:(fun lisp -> print_expr @@ eval lisp)
              |> Option.value ~default:"ERROR"
              |> printer
              (*|> codegen
                |> print_codegen*)
      in
      repl u;
    with
    | e ->
      printer @@ (Exn.to_string e) ^ "\n";
      repl ()

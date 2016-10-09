open Ollvm

open Ollvm.Ez.Value
open Ollvm.Ez.Instr
open Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer

open Core.Std
open Lisp
open Common

let m = M.init
          "scheme_of_ocaml"
          ("x86_64", "pc", "linux-gnu")
          "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

let handle_fn = function
  | List (Atom "+" :: args) -> `Add, args
  | List (Atom "-" :: args) -> `Sub, args
  | List (Atom "/" :: args) -> `Div, args
  | List (Atom "*" :: args) -> `Mul, args
  | _ -> assert false

let get_bin_op = function
  | `Add -> fadd, fadd (Ollvm.Ez.Value.float 0.)
  | `Sub -> fsub, fsub (Ollvm.Ez.Value.float 0.)
  | `Mul -> fmul, fmul (Ollvm.Ez.Value.float 1.)
  | `Div -> fdiv, (fun numerator -> fdiv numerator (Ollvm.Ez.Value.float 1.))


let bin_op_func lisp =
  let fn_expr = handle_fn lisp in
  let args = (snd fn_expr)
    |> List.traverse 
      ~f:(fun lisp -> 
        match lisp with 
        | Number n -> Or_error.return @@ Ollvm.Ez.Value.float n
        | _ -> assert false
      )
    |> Result.ok
    |> Option.value_map
        ~default:[]
        ~f:(fun id -> id)
  in 

  (* variables declaration *)
  let (m, acc) = M.local m T.float "acc" in
  (*let (m, fn_arg) = M.local m T.i32 "" in*)
  let (bin_op, init_instr) = get_bin_op @@ fst fn_expr in
  (* let bin_fn_body_make = List.fold_left
    ~f:(fun instr lisp_arg -> )*)
  let (m, [exec_b]) = M.locals m T.label [""] in 
  let (m, fn) = M.global m T.float "bin_op" in
  let tmp_vars = Array.of_list @@ List.map ~f:(fun x -> snd @@ M.local m T.float "") args in
  (* TODO: correct codegen *)
  let writer_helper idx instrs next_val = 
    let last_var = tmp_vars.(idx) in
    let (_, [var]) = M.locals m T.label [string_of_int (idx + 1)] in
    instrs @ [var <-- bin_op last_var next_val] in
  (* bin func *)
  let f = 
    define fn [] [
      block exec_b ((
        args 
        |> List.tl_exn
        |> List.foldi ~f:writer_helper ~init:[acc <-- init_instr @@ List.hd_exn args]
      ) @ [ret acc])
    ] 
  in
  let m =  M.definition m f in
  m.m_module

let handle_func_def lambda = 
  let (m, fn) = M.global m T.float "__anon__" in
  let f = define fn [] [

  ]
  in
  let m = M.definition m f in
  m.m_module


let codegen = function
  (*| Ok (Number n) -> Or_error.return @@ Ollvm.Ez.Value.float n*)
  | Some Lambda (params, body, env) -> handle_func_def ()
  | Some List ((Atom _) as op :: lisp) when List.length lisp > 1 -> bin_op_func @@ (List (op :: lisp))
  | _ -> failwith "Cant gen from this"

let print_codegen m = 
  P.modul (P.empty_env ()) Format.std_formatter m
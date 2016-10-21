open Core.Std
open Parsec
open Eval
open Driver

module Mock_compiler = struct

  let compile source =
    source
    |> read_sexp
    |> parse_sexp
    |> Result.ok
    |> Option.map ~f:(fun lisp -> evaluate lisp)
    |> Option.value_exn

end

module type Test_mod = sig
  val tests: (string * [> `Quick ] * (unit -> unit)) list
end

module Parse_test: Test_mod = struct
  let test_parse_empty () =
    Alcotest.check_raises "Fails to parse empty" (Failure "Failed to parse input") (fun () -> ignore @@ Mock_compiler.compile "")

  let test_malformed_string () =
    Alcotest.check_raises "Fails to parse bad string" (Failure "Failed to parse input") (fun () -> ignore @@ Mock_compiler.compile "(\")")

  let tests = [
    "Empty input", `Quick, test_parse_empty;
    "Bad String", `Quick, test_malformed_string
  ]
end

let bool_expr_helper expr error_val =
  expr
  |> Mock_compiler.compile
  |> Result.ok
  |> Option.value_map ~default:error_val ~f:(fun lisp -> match lisp with Lisp.Bool b -> b | _ -> assert false)

module Lisp_eq_test: Test_mod = struct

  let test_lisp_equal_sign_nums () =
    Alcotest.(check bool) "Check equal numbers with equal sign" true (bool_expr_helper "(= 2 2)" false)

  let test_lisp_equal_sign_neq_nums () =
    Alcotest.(check bool) "Check not equal numbers with equal sign" false (bool_expr_helper "(= 2 3)" true)

  let test_lisp_equal_sign_atoms () =
    Alcotest.(check bool) "Check equal atoms with equal sign" true (bool_expr_helper "(= '2 '2)" false)

  let test_lisp_equal_sign_neq_atoms () =
    Alcotest.(check bool) "Check not equal atoms with equal sign" false (bool_expr_helper "(= '2 'atom)" true)

  let test_lisp_equal_sign_str () =
    Alcotest.(check bool) "Check equal strings with equal sign" true (bool_expr_helper "(= \"a string\" \"a string\")" false)

  let test_lisp_equal_sign_neq_str () =
    Alcotest.(check bool) "Check not equal strings with equal sign" false (bool_expr_helper "(= \"a string\" \"other string\")" true)

  let test_lisp_equal_sign_list () =
    Alcotest.(check bool) "Check equal lists with equal sign" true (bool_expr_helper "(= (quote 301 'atom \"a string\") (quote 301 'atom \"a string\"))" true)

  let test_lisp_equal_sign_neq_list () =
    Alcotest.(check bool) "Check not equal lists with equal sign" false (bool_expr_helper "(= (quote 1 'atom \"a string\") (quote 301 'atom \"a string\"))" true)

  let test_lisp_equal_sign_multi_expr () =
    (* FIXME *)
    Alcotest.(check bool) "Check equal exprs with equal sign" true (bool_expr_helper "(= 3 3 3 3)" false)

  let tests = [
    "Equal numbers", `Quick, test_lisp_equal_sign_nums;
    "Not equal numbers", `Quick, test_lisp_equal_sign_neq_nums;
    "Equal atoms", `Quick, test_lisp_equal_sign_atoms;
    "Not equal atoms", `Quick, test_lisp_equal_sign_neq_atoms;
    "Equal Strings", `Quick, test_lisp_equal_sign_str;
    "Not equal Strings", `Quick, test_lisp_equal_sign_neq_str;
    "Equal lists", `Quick, test_lisp_equal_sign_list;
    "Not equal lists", `Quick, test_lisp_equal_sign_neq_list;
    "Not equal multi", `Quick, test_lisp_equal_sign_multi_expr;
  ]

end

module Lisp_neq_test: Test_mod = struct
  let test_lisp_equal_sign_nums () =
    Alcotest.(check bool) "Check equal numbers with equal sign" false (bool_expr_helper "(/= 2 2)" true)

  let test_lisp_equal_sign_neq_nums () =
    Alcotest.(check bool) "Check not equal numbers with equal sign" true (bool_expr_helper "(/= 2 3)" false)

  let test_lisp_equal_sign_atoms () =
    Alcotest.(check bool) "Check equal atoms with equal sign" false (bool_expr_helper "(/= '2 '2)" true)

  let test_lisp_equal_sign_neq_atoms () =
    Alcotest.(check bool) "Check not equal atoms with equal sign" true (bool_expr_helper "(/= '2 'atom)" false)

  let test_lisp_equal_sign_str () =
    Alcotest.(check bool) "Check equal strings with equal sign" false (bool_expr_helper "(/= \"a string\" \"a string\")" true)

  let test_lisp_equal_sign_neq_str () =
    Alcotest.(check bool) "Check not equal strings with equal sign" true (bool_expr_helper "(/= \"a string\" \"other string\")" false)

  let test_lisp_equal_sign_list () =
    Alcotest.(check bool) "Check equal lists with equal sign" false (bool_expr_helper "(/= (quote 301 'atom \"a string\") (quote 301 'atom \"a string\"))" true)

  let test_lisp_equal_sign_neq_list () =
    Alcotest.(check bool) "Check not equal lists with equal sign" true (bool_expr_helper "(/= (quote 1 'atom \"a string\") (quote 301 'atom \"a string\"))" false)

  let tests = [
    "Equal numbers", `Quick, test_lisp_equal_sign_nums;
    "Not equal numbers", `Quick, test_lisp_equal_sign_neq_nums;
    "Equal atoms", `Quick, test_lisp_equal_sign_atoms;
    "Not equal atoms", `Quick, test_lisp_equal_sign_neq_atoms;
    "Equal Strings", `Quick, test_lisp_equal_sign_str;
    "Not equal Strings", `Quick, test_lisp_equal_sign_neq_str;
    "Equal lists", `Quick, test_lisp_equal_sign_list;
    "Not equal lists", `Quick, test_lisp_equal_sign_neq_list;
  ]
end

module Lisp_and_test: Test_mod = struct
  let test_lisp_and_bool () =
    Alcotest.(check bool) "check primitive bools" true (bool_expr_helper "(&& true true)" false)

  let test_lisp_not_and_bool () =
    Alcotest.(check bool) "check primitive bools" false (bool_expr_helper "(&& true false)" true)


  let tests = [
    "And both true", `Quick, test_lisp_and_bool;
    "And both not true", `Quick, test_lisp_not_and_bool;
  ]

end

module Lisp_or_test: Test_mod = struct
  let test_lisp_or_bool () =
    Alcotest.(check bool) "check primitive bools" true (bool_expr_helper "(|| true true)" false)

  let test_lisp_or_1_bool () =
    Alcotest.(check bool) "check primitive bools" true (bool_expr_helper "(|| true false)" false)

  let test_lisp_or_2_bool () =
    Alcotest.(check bool) "check primitive bools" true (bool_expr_helper "(|| false true)" false)

  let test_lisp_none_true () =
    Alcotest.(check bool) "check primitive bools" false (bool_expr_helper "(|| false false)" true)


  let tests = [
    "both true", `Quick, test_lisp_or_bool;
    "one is true", `Quick, test_lisp_or_1_bool;
    "one is true", `Quick, test_lisp_or_2_bool;
    "none are true", `Quick, test_lisp_none_true;
  ]
end

let () =
  Alcotest.run "Unit tests" [
    "Parsing", Parse_test.tests;
    "Equal_Operator", Lisp_eq_test.tests;
    "Not_Equal_Operator", Lisp_neq_test.tests;
    "And_Operator", Lisp_and_test.tests;
    "Or_Operator", Lisp_or_test.tests;
  ]

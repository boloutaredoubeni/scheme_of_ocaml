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

module Lisp_test: Alcotest.TESTABLE = struct 
  include Lisp
end

module Fmt = struct 
  include Fmt
  include Fmt.Dump
  let pf = Format.fprintf
  let lisp ppf v = pf ppf "%s" (Lisp.show v) 
end

let lisp = Alcotest.testable Fmt.lisp Lisp.equal

module type TEST_MOD = sig
  val tests: (string * [> `Quick | `Slow ] * (unit -> unit)) list
end

module Parse_test: TEST_MOD = struct
  let test_parse_empty () =
    Alcotest.check_raises "Fails to parse empty" (Failure "Failed to parse input") (fun () -> ignore @@ Mock_compiler.compile "")

  let test_malformed_string () =
    Alcotest.check_raises "Fails to parse bad string" (Failure "Failed to parse input") (fun () -> ignore @@ Mock_compiler.compile "(\")")

  let tests = [
    "Empty input", `Quick, test_parse_empty;
    "Bad String", `Quick, test_malformed_string
  ]
end

let test_helper expr =
  expr
  |> Mock_compiler.compile
  |> Result.ok
  |> Option.value_exn


module Lisp_eq_test: TEST_MOD = struct

  let test_lisp_equal_sign_nums () =
    Alcotest.(check lisp) "Check equal numbers with equal sign" (Lisp.Bool true) (test_helper "(= 2 2)")

  let test_lisp_equal_sign_neq_nums () =
    Alcotest.(check lisp) "Check not equal numbers with equal sign" (Lisp.Bool false) (test_helper "(= 2 3)")

  let test_lisp_equal_sign_atoms () =
    Alcotest.(check lisp) "Check equal atoms with equal sign" (Lisp.Bool true) (test_helper "(= '2 '2)")

  let test_lisp_equal_sign_neq_atoms () =
    Alcotest.(check lisp) "Check not equal atoms with equal sign" (Lisp.Bool false) (test_helper "(= '2 'atom)")

  let test_lisp_equal_sign_str () =
    Alcotest.(check lisp) "Check equal strings with equal sign" (Lisp.Bool true) (test_helper "(= \"a string\" \"a string\")" )

  let test_lisp_equal_sign_neq_str () =
    Alcotest.(check lisp) "Check not equal strings with equal sign" (Lisp.Bool false) (test_helper "(= \"a string\" \"other string\")" )

  let test_lisp_equal_sign_list () =
    Alcotest.(check lisp) "Check equal lists with equal sign" (Lisp.Bool true) (test_helper "(= (quote 301 'atom \"a string\") (quote 301 'atom \"a string\"))")

  let test_lisp_equal_sign_neq_list () =
    Alcotest.(check lisp) "Check not equal lists with equal sign" (Lisp.Bool false) (test_helper "(= (quote 1 'atom \"a string\") (quote 301 'atom \"a string\"))")

  let test_lisp_equal_sign_multi_expr () =
    (* FIXME *)
    Alcotest.(check lisp) "Check equal exprs with equal sign" (Lisp.Bool true) (test_helper "(= 3 3 3 3)")

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

module Lisp_neq_test: TEST_MOD = struct
  let test_lisp_equal_sign_nums () =
    Alcotest.(check lisp) "Check equal numbers with equal sign"  (Lisp.Bool false)  (test_helper "(/= 2 2)")

  let test_lisp_equal_sign_neq_nums () =
    Alcotest.(check lisp) "Check not equal numbers with equal sign" (Lisp.Bool true) (test_helper "(/= 2 3)")

  let test_lisp_equal_sign_atoms () =
    Alcotest.(check lisp) "Check equal atoms with equal sign"  (Lisp.Bool false)  (test_helper "(/= '2 '2)")

  let test_lisp_equal_sign_neq_atoms () =
    Alcotest.(check lisp) "Check not equal atoms with equal sign" (Lisp.Bool true) (test_helper "(/= '2 'atom)")

  let test_lisp_equal_sign_str () =
    Alcotest.(check lisp) "Check equal strings with equal sign"  (Lisp.Bool false)  (test_helper "(/= \"a string\" \"a string\")")

  let test_lisp_equal_sign_neq_str () =
    Alcotest.(check lisp) "Check not equal strings with equal sign" (Lisp.Bool true) (test_helper "(/= \"a string\" \"other string\")")

  let test_lisp_equal_sign_list () =
    Alcotest.(check lisp) "Check equal lists with equal sign"  (Lisp.Bool false)  (test_helper "(/= (quote 301 'atom \"a string\") (quote 301 'atom \"a string\"))")

  let test_lisp_equal_sign_neq_list () =
    Alcotest.(check lisp) "Check not equal lists with equal sign" (Lisp.Bool true) (test_helper "(/= (quote 1 'atom \"a string\") (quote 301 'atom \"a string\"))")

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

module Lisp_and_test: TEST_MOD = struct
  let test_lisp_and_bool () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool true) (test_helper "(&& true true)")

  let test_lisp_not_and_bool () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool false) (test_helper "(&& true false)")


  let tests = [
    "And both true", `Quick, test_lisp_and_bool;
    "And both not true", `Quick, test_lisp_not_and_bool;
  ]

end

module Lisp_or_test: TEST_MOD = struct
  let test_lisp_or_bool () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool true) (test_helper "(|| true true)")

  let test_lisp_or_1_bool () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool true) (test_helper "(|| true false)")

  let test_lisp_or_2_bool () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool true) (test_helper "(|| false true)")

  let test_lisp_none_true () =
    Alcotest.(check lisp) "check primitive bools" (Lisp.Bool false) (test_helper "(|| false false)")


  let tests = [
    "both true", `Quick, test_lisp_or_bool;
    "one is true", `Quick, test_lisp_or_1_bool;
    "one is true", `Quick, test_lisp_or_2_bool;
    "none are true", `Quick, test_lisp_none_true;
  ]
end

module Lisp_binop_test: TEST_MOD = struct

  let test_lisp_add_two_nums () =
    Alcotest.(check lisp) "add numbers" (Lisp.Number 0.) (test_helper "(+ 2 -2 4 -4)")

  let test_lisp_sub_two_nums () =
    Alcotest.(check lisp) "sub numbers" (Lisp.Number 0.) (test_helper "(- 2 2 -4 4)")

  let test_lisp_div_two_nums () =
    Alcotest.(check lisp) "div numbers" (Lisp.Number 4.) (test_helper "(/ 2 (/ 1 2))")

  let test_lisp_multi_two_nums () =
    Alcotest.(check lisp) "multi numbers" (Lisp.Number 120.) (test_helper "(* 5 4 3 2 1)")

  let tests = [
    "add two numbers", `Quick, test_lisp_add_two_nums;
    "sub two numbers", `Quick, test_lisp_sub_two_nums;
    "div two numbers", `Quick, test_lisp_div_two_nums;
    "multi two numbers", `Quick, test_lisp_multi_two_nums;
  ]
end

module Lisp_list_test: TEST_MOD = struct 

  let test_first () =
    Alcotest.(check lisp) "first" (Lisp.String "some sentance of words") (test_helper "(first \"some sentance of words\" 456 'a-symbol)")

  let test_last () =
    let expected = test_helper "(rest \"some sentance of words\" 456 'a-symbol)" in
    let actual = test_helper  "(quote 456 'a-symbol)" in
    Alcotest.(check lisp) "last" expected actual

  let test_cons () =
    let expected = test_helper "(quote \"some sentance of words\" 456 'a-symbol)" in
    let actual = test_helper  "(cons \"some sentance of words\" (cons 456 'a-symbol))" in
    Alcotest.(check lisp) "cons" expected actual

  let tests = [
    "get the head", `Quick, test_first;
    "get the tail", `Quick, test_last;
    "make a list", `Quick, test_cons;
  ]
end

module Lisp_define_and_function_test: TEST_MOD = struct 

  

  let tests = [

  ]
end

let () =
  Alcotest.run "Unit tests" [
    "Parsing", Parse_test.tests;
    "Equal_Operator", Lisp_eq_test.tests;
    "Not_Equal_Operator", Lisp_neq_test.tests;
    "And_Operator", Lisp_and_test.tests;
    "Or_Operator", Lisp_or_test.tests;
    "Binop_Operators", Lisp_binop_test.tests;
    "List operations", Lisp_list_test.tests;
    "Lisp functions and define", Lisp_define_and_function_test.tests;
  ]

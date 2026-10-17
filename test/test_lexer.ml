(* Unit tests for the lexer: checks the exact token list for valid input
   and that invalid input raises Lex_error. Run with: dune test *)

open Compiler.Lexer

let failures = ref 0

let show tokens = String.concat " " (List.map string_of_token tokens)

let expect_tokens name src expected =
  match lex src with
  | actual when actual = expected -> Printf.printf "ok   %s\n" name
  | actual ->
      incr failures;
      Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n" name
        (show expected) (show actual)
  | exception Lex_error msg ->
      incr failures;
      Printf.printf "FAIL %s\n  unexpected Lex_error: %s\n" name msg

let expect_error name src =
  match lex src with
  | tokens ->
      incr failures;
      Printf.printf "FAIL %s\n  expected Lex_error, got: %s\n" name
        (show tokens)
  | exception Lex_error msg -> Printf.printf "ok   %s (%s)\n" name msg

let return_2 =
  [ KwInt; Identifier "main"; OpenParen; KwVoid; CloseParen; OpenBrace;
    KwReturn; Constant 2; Semicolon; CloseBrace ]

let () =
  expect_tokens "return 2" "int main(void) {\n  return 2;\n}\n" return_2;
  expect_tokens "no whitespace" "int main(void){return 2;}" return_2;
  expect_tokens "tabs and newlines" "int\tmain\n(\nvoid\n)\t{return\n2\n;}"
    return_2;
  expect_tokens "multi-digit constant" "100" [ Constant 100 ];
  expect_tokens "empty input" "" [];
  expect_tokens "keyword prefix is identifier" "returnx int_ voids"
    [ Identifier "returnx"; Identifier "int_"; Identifier "voids" ];
  expect_tokens "keywords are case sensitive" "RETURN Int"
    [ Identifier "RETURN"; Identifier "Int" ];
  expect_tokens "underscore identifier" "_foo1" [ Identifier "_foo1" ];
  expect_error "at sign" "int main(void) { return 0@1; }";
  expect_error "backslash" "\\";
  expect_error "backtick" "`";
  expect_error "identifier starting with digit" "return 1foo;";
  expect_error "digit followed by underscore" "123_bar";
  if !failures > 0 then (
    Printf.printf "\n%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "\nall lexer tests passed"

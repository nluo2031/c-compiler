exception Parse_error of string
let take_token (tokens: Lexer.token list ref) :Lexer.token=
  match !tokens with
  | [] -> raise (Parse_error "unexpected end of file")
  | t :: rest ->
      tokens := rest;
      t
let expect (expected :Lexer.token) (tokens : Lexer.token list ref) : unit =
  let actual = take_token tokens in
  if actual <> expected then
    raise
      (Parse_error
         (Printf.sprintf "expected %s but found %s"
            (Lexer.string_of_token expected)
            (Lexer.string_of_token actual))) 
let parse_exp (tokens:Lexer.token list ref) : Ast.exp=
    match take_token tokens with
      | Lexer.Constant n -> Ast.Constant n
      | t ->
        raise
          (Parse_error
            (Printf.sprintf "expected a constant but found %s"
                (Lexer.string_of_token t)))
let parse_statement (tokens : Lexer.token list ref) : Ast.statement =
  expect Lexer.KwReturn tokens;
  let return_val = parse_exp tokens in
  expect Lexer.Semicolon tokens;
  Ast.Return return_val
let parse_identifier (tokens : Lexer.token list ref) : string =
  match take_token tokens with
  | Lexer.Identifier name -> name
  | t ->
      raise
        (Parse_error
           (Printf.sprintf "expected an identifier but found %s"
              (Lexer.string_of_token t)))


let parse_function (tokens : Lexer.token list ref) : Ast.function_definition =
  expect Lexer.KwInt tokens;
  let name = parse_identifier tokens in
  expect Lexer.OpenParen tokens;
  expect Lexer.KwVoid tokens;
  expect Lexer.CloseParen tokens;
  expect Lexer.OpenBrace tokens;
  let body = parse_statement tokens in
  expect Lexer.CloseBrace tokens;
  Ast.Function (name, body)

let parse_program (token_list : Lexer.token list) : Ast.program =
  let tokens = ref token_list in
  let f = parse_function tokens in
  match !tokens with
  | [] -> Ast.Program f
  | t :: _ ->
      raise
        (Parse_error
           (Printf.sprintf "unexpected %s after end of function"
              (Lexer.string_of_token t)))
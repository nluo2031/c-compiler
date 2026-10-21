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

let rec parse_exp (tokens:Lexer.token list ref) : Ast.exp=
    match take_token tokens with
      | Lexer.Constant n -> Ast.Constant n
      | Lexer.Tilde -> Ast.Unary(Ast.Complement, parse_exp tokens)
      |Lexer.Hyphen -> Ast.Unary(Ast.Negate, parse_exp tokens)
      | Lexer.OpenParen ->
          let inner = parse_exp tokens in
          expect Lexer.CloseParen tokens;
          inner
      | t ->
        raise
          (Parse_error
            (Printf.sprintf "expected an expression but found %s"
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

(* ---------- Pretty-printer ---------- *)

(* Two spaces per indentation level. *)
let indent (level : int) : string = String.make (2 * level) ' '

(* Each function returns a string whose first line has no indentation
   (the caller has already positioned it), and whose later lines are
   indented relative to `level`. *)

let rec pp_exp (level : int) (e : Ast.exp) : string =
  match e with
  | Ast.Constant n -> Printf.sprintf "Constant(%d)" n
  | Ast.Unary (op, inner) ->
      let op_name = match op with Ast.Complement -> "Complement" | Ast.Negate -> "Negate" in
      Printf.sprintf "Unary(%s, %s)" op_name (pp_exp level inner)

let pp_statement (level : int) (s : Ast.statement) : string =
  match s with
  | Ast.Return e ->
      Printf.sprintf "Return(\n%s%s\n%s)"
        (indent (level + 1)) (pp_exp (level + 1) e)
        (indent level)

let pp_function (level : int) (f : Ast.function_definition) : string =
  match f with
  | Ast.Function (name, body) ->
      Printf.sprintf "Function(\n%sname=%S,\n%sbody=%s\n%s)"
        (indent (level + 1)) name
        (indent (level + 1)) (pp_statement (level + 1) body)
        (indent level)

let pp_program (p : Ast.program) : string =
  match p with
  | Ast.Program f -> Printf.sprintf "Program(\n%s%s\n)" (indent 1) (pp_function 1 f)
exception Parse_error of string
let take_token (tokens: Lexer.token list ref) :Lexer.token=
  match !tokens with
  | [] -> raise (Parse_error "unexpected end of file")
  | t :: rest ->
      tokens := rest;
      t
let peek (tokens:Lexer.token list ref): Lexer.token option=
    match !tokens with
    | [] -> None
    | t :: _ -> Some t
let priority (token: Lexer.token): int option=
    match token with
    | Lexer.Times | Lexer.FwSlash | Lexer.Mod -> Some 50
    | Lexer.Plus | Lexer.Hyphen -> Some 45
    | Lexer.DoubleLess | Lexer.DoubleGreater -> Some 40
    | Lexer.Ampersand -> Some 25
    | Lexer.Caret -> Some 20
    | Lexer.Pipe -> Some 15
    | _ -> None
let binop_of_token (token : Lexer.token) : Ast.binary_operator =
  match token with
  | Lexer.Plus -> Ast.Add
  | Lexer.Times -> Ast.Multiply
  | Lexer.FwSlash -> Ast.Divide
  | Lexer.Mod -> Ast.Remainder
  | Lexer.Hyphen -> Ast.Subtract
  | Lexer.Ampersand -> Ast.BitwiseAnd
  | Lexer.Pipe -> Ast.BitwiseOr
  | Lexer.Caret -> Ast.BitwiseXor
  | Lexer.DoubleLess -> Ast.LeftShift
  | Lexer.DoubleGreater -> Ast.RightShift
  | _ -> raise (Parse_error "expected arithmetic")


let expect (expected :Lexer.token) (tokens : Lexer.token list ref) : unit =
  let actual = take_token tokens in
  if actual <> expected then
    raise
      (Parse_error
         (Printf.sprintf "expected %s but found %s"
            (Lexer.string_of_token expected)
            (Lexer.string_of_token actual))) 

let rec parse_factor (tokens:Lexer.token list ref) : Ast.exp=
    match take_token tokens with
      | Lexer.Constant n -> Ast.Constant n
      | Lexer.Tilde -> Ast.Unary(Ast.Complement, parse_factor tokens)
      |Lexer.Hyphen -> Ast.Unary(Ast.Negate, parse_factor tokens)
      | Lexer.OpenParen ->
          let inner = parse_exp tokens 0 in
          expect Lexer.CloseParen tokens;
          inner
      | t ->
        raise
          (Parse_error
            (Printf.sprintf "expected an expression but found %s"
                (Lexer.string_of_token t)))
and parse_exp (tokens:Lexer.token list ref) (min_prec:int) : Ast.exp=
        let rec loop left =
          match peek tokens with
           | Some t ->
              (match priority t with
              | Some prec when prec >= min_prec ->
                let _ = take_token tokens in
                let op = binop_of_token t in
                let right = parse_exp tokens (prec + 1) in
                loop (Ast.Binary (op, left, right))
              | _ -> left)
            | None -> left
          in
            loop (parse_factor tokens)






let parse_statement (tokens : Lexer.token list ref) : Ast.statement =
  expect Lexer.KwReturn tokens;
  let return_val = parse_exp tokens 0 in
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
  | Ast.Binary (op, left, right) ->
      let op_name =
        match op with
        | Ast.Add -> "Add"
        | Ast.Subtract -> "Subtract"
        | Ast.Multiply -> "Multiply"
        | Ast.Divide -> "Divide"
        | Ast.Remainder -> "Remainder"
        | Ast.BitwiseAnd -> "BitwiseAnd"
        | Ast.BitwiseOr -> "BitwiseOr"
        | Ast.BitwiseXor -> "BitwiseXor"
        | Ast.LeftShift -> "LeftShift"
        | Ast.RightShift -> "RightShift"
      in
      Printf.sprintf "Binary(%s, %s, %s)" op_name (pp_exp level left) (pp_exp level right)

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
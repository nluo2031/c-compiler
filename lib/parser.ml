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
    | Lexer.Less | Lexer.LessEqual | Lexer.Greater | Lexer.GreaterEqual -> Some 35
    | Lexer.DoubleEqual | Lexer.BangEqual -> Some 30
    | Lexer.Ampersand -> Some 25
    | Lexer.Caret -> Some 20
    | Lexer.Pipe -> Some 15
    | Lexer.DoubleAmpersand -> Some 10
    | Lexer.DoublePipe -> Some 5
    | Lexer.Question -> Some 3
    | Lexer.Equal -> Some 1
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
  | Lexer.DoubleAmpersand -> Ast.And
  | Lexer.DoublePipe -> Ast.Or
  | Lexer.DoubleEqual -> Ast.Equal
  | Lexer.BangEqual -> Ast.NotEqual
  | Lexer.Less -> Ast.LessThan
  | Lexer.LessEqual -> Ast.LessOrEqual
  | Lexer.Greater -> Ast.GreaterThan
  | Lexer.GreaterEqual -> Ast.GreaterOrEqual
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
      | Lexer.Identifier name -> Ast.Var name
      | Lexer.Tilde -> Ast.Unary(Ast.Complement, parse_factor tokens)
      |Lexer.Hyphen -> Ast.Unary(Ast.Negate, parse_factor tokens)
      | Lexer.Bang -> Ast.Unary(Ast.Not, parse_factor tokens)
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
                if t = Lexer.Equal then
                  (* right-associative: a = b = c parses as a = (b = c) *)
                  let right = parse_exp tokens prec in
                  loop (Ast.Assignment (left, right))
                else if t = Lexer.Question then
                  (* the ? and : delimit the middle like parentheses, so any expression fits there *)
                  let middle = parse_exp tokens 0 in
                  expect Lexer.Colon tokens;
                  (* right-associative: a ? 1 : b ? 2 : 3 parses as a ? 1 : (b ? 2 : 3) *)
                  let right = parse_exp tokens prec in
                  loop (Ast.Conditional (left, middle, right))
                else
                  let op = binop_of_token t in
                  let right = parse_exp tokens (prec + 1) in
                  loop (Ast.Binary (op, left, right))
              | _ -> left)
            | None -> left
          in
            loop (parse_factor tokens)






let rec parse_statement (tokens : Lexer.token list ref) : Ast.statement =
  match peek tokens with
  | Some Lexer.KwReturn ->
      let _ = take_token tokens in
      let return_val = parse_exp tokens 0 in
      expect Lexer.Semicolon tokens;
      Ast.Return return_val
  | Some Lexer.KwIf ->
      let _ = take_token tokens in
      expect Lexer.OpenParen tokens;
      let condition = parse_exp tokens 0 in
      expect Lexer.CloseParen tokens;
      let then_branch = parse_statement tokens in
      (* checking right after the body binds a dangling else to the closest if *)
      let else_branch =
        match peek tokens with
        | Some Lexer.KwElse ->
            let _ = take_token tokens in
            Some (parse_statement tokens)
        | _ -> None
      in
      Ast.If (condition, then_branch, else_branch)
  | Some Lexer.Semicolon ->
      let _ = take_token tokens in
      Ast.Null
  | _ ->
      let e = parse_exp tokens 0 in
      expect Lexer.Semicolon tokens;
      Ast.Expression e
let parse_identifier (tokens : Lexer.token list ref) : string =
  match take_token tokens with
  | Lexer.Identifier name -> name
  | t ->
      raise
        (Parse_error
           (Printf.sprintf "expected an identifier but found %s"
              (Lexer.string_of_token t)))

let parse_declaration (tokens : Lexer.token list ref) : Ast.declaration =
  expect Lexer.KwInt tokens;
  let name = parse_identifier tokens in
  match take_token tokens with
  | Lexer.Semicolon -> Ast.Declaration (name, None)
  | Lexer.Equal ->
      let init = parse_exp tokens 0 in
      expect Lexer.Semicolon tokens;
      Ast.Declaration (name, Some init)
  | t ->
      raise
        (Parse_error
           (Printf.sprintf "expected = or ; but found %s"
              (Lexer.string_of_token t)))

let parse_block_item (tokens : Lexer.token list ref) : Ast.block_item =
  match peek tokens with
  | Some Lexer.KwInt -> Ast.D (parse_declaration tokens)
  | _ -> Ast.S (parse_statement tokens)

let rec parse_block_items (tokens : Lexer.token list ref) : Ast.block_item list =
  match peek tokens with
  | Some Lexer.CloseBrace -> []
  | _ ->
      let item = parse_block_item tokens in
      item :: parse_block_items tokens

let parse_function (tokens : Lexer.token list ref) : Ast.function_definition =
  expect Lexer.KwInt tokens;
  let name = parse_identifier tokens in
  expect Lexer.OpenParen tokens;
  expect Lexer.KwVoid tokens;
  expect Lexer.CloseParen tokens;
  expect Lexer.OpenBrace tokens;
  let body = parse_block_items tokens in
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
  | Ast.Var name -> Printf.sprintf "Var(%S)" name
  | Ast.Assignment (left, right) ->
      Printf.sprintf "Assignment(%s, %s)" (pp_exp level left) (pp_exp level right)
  | Ast.Conditional (condition, e1, e2) ->
      Printf.sprintf "Conditional(%s, %s, %s)" (pp_exp level condition) (pp_exp level e1) (pp_exp level e2)
  | Ast.Unary (op, inner) ->
      let op_name = match op with Ast.Complement -> "Complement" | Ast.Negate -> "Negate" | Ast.Not -> "Not" in
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
        | Ast.And -> "And"
        | Ast.Or -> "Or"
        | Ast.Equal -> "Equal"
        | Ast.NotEqual -> "NotEqual"
        | Ast.LessThan -> "LessThan"
        | Ast.LessOrEqual -> "LessOrEqual"
        | Ast.GreaterThan -> "GreaterThan"
        | Ast.GreaterOrEqual -> "GreaterOrEqual"
      in
      Printf.sprintf "Binary(%s, %s, %s)" op_name (pp_exp level left) (pp_exp level right)

let rec pp_statement (level : int) (s : Ast.statement) : string =
  match s with
  | Ast.Return e ->
      Printf.sprintf "Return(\n%s%s\n%s)"
        (indent (level + 1)) (pp_exp (level + 1) e)
        (indent level)
  | Ast.Expression e ->
      Printf.sprintf "Expression(%s)" (pp_exp level e)
  | Ast.If (condition, then_branch, else_branch) ->
      let else_text =
        match else_branch with
        | None -> ""
        | Some s ->
            Printf.sprintf ",\n%selse=%s" (indent (level + 1)) (pp_statement (level + 1) s)
      in
      Printf.sprintf "If(\n%scondition=%s,\n%sthen=%s%s\n%s)"
        (indent (level + 1)) (pp_exp (level + 1) condition)
        (indent (level + 1)) (pp_statement (level + 1) then_branch)
        else_text
        (indent level)
  | Ast.Null -> "Null"

let pp_declaration (level : int) (d : Ast.declaration) : string =
  match d with
  | Ast.Declaration (name, None) -> Printf.sprintf "Declaration(%S)" name
  | Ast.Declaration (name, Some init) ->
      Printf.sprintf "Declaration(%S, %s)" name (pp_exp level init)

let pp_block_item (level : int) (item : Ast.block_item) : string =
  match item with
  | Ast.S s -> pp_statement level s
  | Ast.D d -> pp_declaration level d

let pp_function (level : int) (f : Ast.function_definition) : string =
  match f with
  | Ast.Function (name, body) ->
      let items =
        List.map
          (fun item -> indent (level + 2) ^ pp_block_item (level + 2) item)
          body
      in
      Printf.sprintf "Function(\n%sname=%S,\n%sbody=[\n%s\n%s]\n%s)"
        (indent (level + 1)) name
        (indent (level + 1)) (String.concat ",\n" items)
        (indent (level + 1))
        (indent level)

let pp_program (p : Ast.program) : string =
  match p with
  | Ast.Program f -> Printf.sprintf "Program(\n%s%s\n)" (indent 1) (pp_function 1 f)
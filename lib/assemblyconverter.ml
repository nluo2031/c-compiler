let convert_exp (e:Ast.exp):Assembly.operand=
  match e with
  | Ast.Constant n -> Assembly.Imm n
let convert_statement (s:Ast.statement):Assembly.instruction list=
  match s with
  |Ast.Return e -> [ Assembly.Mov (convert_exp e, Assembly.Register); Assembly.Ret ]

let convert_function (f : Ast.function_definition) : Assembly.function_definition =
  match f with
  | Ast.Function (name, body) -> Assembly.Function (name, convert_statement body)

let convert_program (p : Ast.program) : Assembly.program =
  match p with
  | Ast.Program f -> Assembly.Program (convert_function f)
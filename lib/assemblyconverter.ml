let convert_val (e:Tacky.tacky_val):Assembly.operand=
  match e with
  | Tacky.Constant n -> Assembly.Imm n
  | Tacky.Var name -> Assembly.Pseudo name
let convert_unop (op : Tacky.unary_operator) : Assembly.unary_operator =
  match op with
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
let convert_instruction (s:Tacky.instruction):Assembly.instruction list=
  match s with
  |Tacky.Return e -> [ Assembly.Mov (convert_val e, Assembly.Reg Assembly.AX); Assembly.Ret ] 
  |Tacky.Unary (op,src,dst)-> [Assembly.Mov(convert_val src, convert_val dst);Assembly.Unary (convert_unop op, convert_val dst)] 

let convert_function (f : Tacky.function_definition) : Assembly.function_definition =
  match f with
  | Tacky.Function (name, instrs) -> 
      Assembly.Function (name, List.concat_map convert_instruction instrs)

let convert_program (p : Tacky.program) : Assembly.program =
  match p with
  | Tacky.Program f -> Assembly.Program (convert_function f)


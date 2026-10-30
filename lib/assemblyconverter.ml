let convert_val (e:Tacky.tacky_val):Assembly.operand=
  match e with
  | Tacky.Constant n -> Assembly.Imm n
  | Tacky.Var name -> Assembly.Pseudo name
let convert_binary (b: Tacky.binary_operator):Assembly.binary_operator=
  match b with
  | Tacky.Add->Assembly.Add
  | Tacky.Subtract->Assembly.Sub
  | Tacky.Multiply-> Assembly.Mult
  | Tacky.BitwiseAnd -> Assembly.And
  | Tacky.BitwiseOr -> Assembly.Or
  | Tacky.BitwiseXor -> Assembly.Xor
  | Tacky.LeftShift -> Assembly.Sal
  | Tacky.RightShift -> Assembly.Sar
  | Tacky.Divide | Tacky.Remainder -> failwith "handled separately"
  | Tacky.Equal | Tacky.NotEqual | Tacky.LessThan | Tacky.LessOrEqual
  | Tacky.GreaterThan | Tacky.GreaterOrEqual -> failwith "TODO chapter 4: comparisons"

let convert_cond (b: Tacky.binary_operator):Assembly.cond_code=
  match b with
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.LessThan -> Assembly.L
  | Tacky.LessOrEqual -> Assembly.LE
  | Tacky.GreaterThan -> Assembly.G
  | Tacky.GreaterOrEqual -> Assembly.GE
  | _ -> failwith "not a comparison"

let convert_unop (op : Tacky.unary_operator) : Assembly.unary_operator =
  match op with
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
  | Tacky.Not -> failwith "TODO chapter 4: logical not"
let convert_instruction (s:Tacky.instruction):Assembly.instruction list=
  match s with
  |Tacky.Return e -> [ Assembly.Mov (convert_val e, Assembly.Reg Assembly.AX); Assembly.Ret ] 
  |Tacky.Unary (op,src,dst)-> [Assembly.Mov(convert_val src, convert_val dst);Assembly.Unary (convert_unop op, convert_val dst)] 
  |Tacky.Binary (Tacky.Divide, src1,src2,dst)->
    [ Assembly.Mov (convert_val src1, Assembly.Reg Assembly.AX);
      Assembly.Cdq;
      Assembly.Idiv (convert_val src2);
      Assembly.Mov (Assembly.Reg Assembly.AX, convert_val dst) ]
  |Tacky.Binary (Tacky.Remainder, src1,src2,dst)->
    [Assembly.Mov (convert_val src1, Assembly.Reg Assembly.AX);
    Assembly.Cdq;
    Assembly.Idiv (convert_val src2);
    Assembly.Mov (Assembly.Reg Assembly.DX, convert_val dst) ]
  |Tacky.Binary (op, src1, src2, dst)->
    [Assembly.Mov (convert_val src1, convert_val dst);
    Assembly.Binary (convert_binary op, convert_val src2, convert_val dst)]
  |Tacky.Copy (src,dst)->[Assembly.Mov (convert_val src, convert_val dst)]
  |Tacky.Jump _|Tacky.JumpIfZero _|Tacky.JumpIfNotZero _|Tacky.Label _->
    failwith "TODO chapter 4: jumps and labels"

let convert_function (f : Tacky.function_definition) : Assembly.function_definition =
  match f with
  | Tacky.Function (name, instrs) -> 
      Assembly.Function (name, List.concat_map convert_instruction instrs)

let convert_program (p : Tacky.program) : Assembly.program =
  match p with
  | Tacky.Program f -> Assembly.Program (convert_function f)


let convert_operator (op:Ast.unary_operator):Tacky.unary_operator=
    match op with
    | Ast.Complement->Tacky.Complement
    |Ast.Negate->Tacky.Negate
let convert_binop (op:Ast.binary_operator):Tacky.binary_operator=
    match op with
    |Ast.Add->Tacky.Add
    |Ast.Subtract->Tacky.Subtract
    |Ast.Multiply->Tacky.Multiply
    |Ast.Divide->Tacky.Divide
    |Ast.Remainder->Tacky.Remainder
    |Ast.BitwiseAnd->Tacky.BitwiseAnd
    |Ast.BitwiseOr->Tacky.BitwiseOr
    |Ast.BitwiseXor->Tacky.BitwiseXor
    |Ast.LeftShift->Tacky.LeftShift
    |Ast.RightShift->Tacky.RightShift
let counter =ref 0
let make_temp ():string=
  incr counter;
  "tmp."^string_of_int !counter
let rec expression_conversion (exp:Ast.exp):Tacky.instruction list*Tacky.tacky_val=
  match exp with
  |Ast.Constant n->([],Tacky.Constant n)
  |Ast.Unary (op,inner)-> 
    let (inner_instrs, src) = expression_conversion inner in
    let dst = Tacky.Var (make_temp ()) in
    (inner_instrs @ [Tacky.Unary (convert_operator op, src, dst)], dst)
  |Ast.Binary (op,left,right)->
    (* left must be evaluated before right *)
    let (left_instrs, src1) = expression_conversion left in
    let (right_instrs, src2) = expression_conversion right in
    let dst = Tacky.Var (make_temp ()) in
    (left_instrs @ right_instrs @ [Tacky.Binary (convert_binop op, src1, src2, dst)], dst)

let statement_conversion (stmt:Ast.statement):Tacky.instruction list=
  match stmt with
  |Ast.Return e->
    let (instrs, result) = expression_conversion e in
    instrs @ [Tacky.Return result]
let program_conversion (prog:Ast.program):Tacky.program=
  match prog with
  |Ast.Program (Ast.Function (name, body))->
    Tacky.Program (Tacky.Function (name, statement_conversion body))

(* ---------- Pretty-printer ---------- *)
let pp_operator (op:Tacky.unary_operator):string=
  match op with
  |Tacky.Complement->"Complement"
  |Tacky.Negate->"Negate"
let pp_binop (op:Tacky.binary_operator):string=
  match op with
  |Tacky.Add->"Add"
  |Tacky.Subtract->"Subtract"
  |Tacky.Multiply->"Multiply"
  |Tacky.Divide->"Divide"
  |Tacky.Remainder->"Remainder"
  |Tacky.BitwiseAnd->"BitwiseAnd"
  |Tacky.BitwiseOr->"BitwiseOr"
  |Tacky.BitwiseXor->"BitwiseXor"
  |Tacky.LeftShift->"LeftShift"
  |Tacky.RightShift->"RightShift"
let pp_val (v:Tacky.tacky_val):string=
  match v with
  |Tacky.Constant n->"Constant "^string_of_int n
  |Tacky.Var name->"Var "^name
let pp_instruction (instr:Tacky.instruction):string=
  match instr with
  |Tacky.Return v->Printf.sprintf "Return(%s)" (pp_val v)
  |Tacky.Unary (op,src,dst)->
    Printf.sprintf "Unary(%s, %s, %s)" (pp_operator op) (pp_val src) (pp_val dst)
  |Tacky.Binary (op,src1,src2,dst)->
    Printf.sprintf "Binary(%s, %s, %s, %s)" (pp_binop op) (pp_val src1) (pp_val src2) (pp_val dst)
let pp_program (prog:Tacky.program):string=
  match prog with
  |Tacky.Program (Tacky.Function (_, instrs))->
    String.concat "\n" (List.map pp_instruction instrs)

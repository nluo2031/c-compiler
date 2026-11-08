let convert_operator (op:Ast.unary_operator):Tacky.unary_operator=
    match op with
    | Ast.Complement->Tacky.Complement
    |Ast.Negate->Tacky.Negate
    |Ast.Not->Tacky.Not
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
    |Ast.Equal->Tacky.Equal
    |Ast.NotEqual->Tacky.NotEqual
    |Ast.LessThan->Tacky.LessThan
    |Ast.LessOrEqual->Tacky.LessOrEqual
    |Ast.GreaterThan->Tacky.GreaterThan
    |Ast.GreaterOrEqual->Tacky.GreaterOrEqual
    |Ast.And|Ast.Or->failwith "And/Or short-circuit, handled separately"
let counter =ref 0
let make_temp ():string=
  incr counter;
  "tmp."^string_of_int !counter

let make_label (prefix:string):string=
  incr counter;
  prefix^"."^string_of_int !counter
let rec expression_conversion (exp:Ast.exp):Tacky.instruction list*Tacky.tacky_val=
  match exp with
  |Ast.Constant n->([],Tacky.Constant n)
  |Ast.Var name->([],Tacky.Var name)
  |Ast.Assignment (Ast.Var name,right)->
    let (right_instrs, result) = expression_conversion right in
    let dst = Tacky.Var name in
    (right_instrs @ [Tacky.Copy (result, dst)], dst)
  (* x op= e becomes x = x op e; the result is x's new value *)
  |Ast.CompoundAssignment (op,Ast.Var name,right)->
    let (right_instrs, result) = expression_conversion right in
    let dst = Tacky.Var name in
    (right_instrs @ [Tacky.Binary (convert_binop op, dst, result, dst)], dst)
  |Ast.Assignment _|Ast.CompoundAssignment _->failwith "invalid lvalue, should be rejected by semantic analysis"
  (* only the chosen clause may run, so each one gets its own branch *)
  |Ast.Conditional (condition,e1,e2)->
    let (condition_instrs, c) = expression_conversion condition in
    let (e1_instrs, v1) = expression_conversion e1 in
    let (e2_instrs, v2) = expression_conversion e2 in
    let e2_label = make_label "conditional_e2" in
    let end_label = make_label "conditional_end" in
    let dst = Tacky.Var (make_temp ()) in
    (condition_instrs @ [Tacky.JumpIfZero (c, e2_label)]
     @ e1_instrs @ [Tacky.Copy (v1, dst);
       Tacky.Jump end_label;
       Tacky.Label e2_label]
     @ e2_instrs @ [Tacky.Copy (v2, dst);
       Tacky.Label end_label], dst)
  |Ast.Unary (op,inner)-> 
    let (inner_instrs, src) = expression_conversion inner in
    let dst = Tacky.Var (make_temp ()) in
    (inner_instrs @ [Tacky.Unary (convert_operator op, src, dst)], dst)
  (* a && b: if either side is 0, jump straight to the false case without evaluating the rest *)
  |Ast.Binary (Ast.And,left,right)->
    let (left_instrs, v1) = expression_conversion left in
    let (right_instrs, v2) = expression_conversion right in
    let false_label = make_label "and_false" in
    let end_label = make_label "and_end" in
    let dst = Tacky.Var (make_temp ()) in
    (left_instrs @ [Tacky.JumpIfZero (v1, false_label)]
     @ right_instrs @ [Tacky.JumpIfZero (v2, false_label);
       Tacky.Copy (Tacky.Constant 1, dst);
       Tacky.Jump end_label;
       Tacky.Label false_label;
       Tacky.Copy (Tacky.Constant 0, dst);
       Tacky.Label end_label], dst)
  (* a || b: if either side is nonzero, jump straight to the true case *)
  |Ast.Binary (Ast.Or,left,right)->
    let (left_instrs, v1) = expression_conversion left in
    let (right_instrs, v2) = expression_conversion right in
    let true_label = make_label "or_true" in
    let end_label = make_label "or_end" in
    let dst = Tacky.Var (make_temp ()) in
    (left_instrs @ [Tacky.JumpIfNotZero (v1, true_label)]
     @ right_instrs @ [Tacky.JumpIfNotZero (v2, true_label);
       Tacky.Copy (Tacky.Constant 0, dst);
       Tacky.Jump end_label;
       Tacky.Label true_label;
       Tacky.Copy (Tacky.Constant 1, dst);
       Tacky.Label end_label], dst)
  |Ast.Binary (op,left,right)->
    (* left must be evaluated before right *)
    let (left_instrs, src1) = expression_conversion left in
    let (right_instrs, src2) = expression_conversion right in
    let dst = Tacky.Var (make_temp ()) in
    (left_instrs @ right_instrs @ [Tacky.Binary (convert_binop op, src1, src2, dst)], dst)

(* a declaration only produces code if it has an initializer *)
let declaration_conversion (decl:Ast.declaration):Tacky.instruction list=
  match decl with
  |Ast.Declaration (_, None)->[]
  |Ast.Declaration (name, Some init)->
    let (instrs, _) = expression_conversion (Ast.Assignment (Ast.Var name, init)) in
    instrs

(* a loop's label gives the targets that break and continue jump to *)
let break_label (label:string):string = "break_" ^ label
let continue_label (label:string):string = "continue_" ^ label

let opt_expression_conversion (e:Ast.exp option):Tacky.instruction list=
  match e with
  |None->[]
  |Some e->fst (expression_conversion e)

let rec statement_conversion (stmt:Ast.statement):Tacky.instruction list=
  match stmt with
  |Ast.Return e->
    let (instrs, result) = expression_conversion e in
    instrs @ [Tacky.Return result]
  |Ast.Expression e->
    let (instrs, _) = expression_conversion e in
    instrs
  |Ast.If (condition,then_branch,None)->
    let (condition_instrs, c) = expression_conversion condition in
    let end_label = make_label "if_end" in
    condition_instrs @ [Tacky.JumpIfZero (c, end_label)]
    @ statement_conversion then_branch @ [Tacky.Label end_label]
  |Ast.If (condition,then_branch,Some else_branch)->
    let (condition_instrs, c) = expression_conversion condition in
    let else_label = make_label "if_else" in
    let end_label = make_label "if_end" in
    condition_instrs @ [Tacky.JumpIfZero (c, else_label)]
    @ statement_conversion then_branch
    @ [Tacky.Jump end_label; Tacky.Label else_label]
    @ statement_conversion else_branch @ [Tacky.Label end_label]
  |Ast.Compound b->block_conversion b
  |Ast.Break label->[Tacky.Jump (break_label label)]
  |Ast.Continue label->[Tacky.Jump (continue_label label)]
  (* the condition is tested at the bottom, so the body always runs once *)
  |Ast.DoWhile (body,condition,label)->
    let start = make_label "do_start" in
    let (condition_instrs, c) = expression_conversion condition in
    [Tacky.Label start] @ statement_conversion body
    @ [Tacky.Label (continue_label label)]
    @ condition_instrs
    @ [Tacky.JumpIfNotZero (c, start); Tacky.Label (break_label label)]
  (* continue re-tests the condition, so it can share the loop's top label *)
  |Ast.While (condition,body,label)->
    let (condition_instrs, c) = expression_conversion condition in
    [Tacky.Label (continue_label label)] @ condition_instrs
    @ [Tacky.JumpIfZero (c, break_label label)]
    @ statement_conversion body
    @ [Tacky.Jump (continue_label label); Tacky.Label (break_label label)]
  (* continue must still run the post expression, so it lands just before it *)
  |Ast.For (init,condition,post,body,label)->
    let start = make_label "for_start" in
    let init_instrs =
      match init with
      |Ast.InitDecl d->declaration_conversion d
      |Ast.InitExp e->opt_expression_conversion e
    in
    (* a missing condition means loop forever *)
    let condition_instrs =
      match condition with
      |None->[]
      |Some condition->
        let (instrs, c) = expression_conversion condition in
        instrs @ [Tacky.JumpIfZero (c, break_label label)]
    in
    init_instrs @ [Tacky.Label start] @ condition_instrs
    @ statement_conversion body
    @ [Tacky.Label (continue_label label)]
    @ opt_expression_conversion post
    @ [Tacky.Jump start; Tacky.Label (break_label label)]
  |Ast.Null->[]

and block_item_conversion (item:Ast.block_item):Tacky.instruction list=
  match item with
  |Ast.S stmt->statement_conversion stmt
  |Ast.D decl->declaration_conversion decl

and block_conversion (b:Ast.block):Tacky.instruction list=
  match b with
  |Ast.Block items->List.concat_map block_item_conversion items

let program_conversion (prog:Ast.program):Tacky.program=
  match prog with
  |Ast.Program (Ast.Function (name, body))->
    (* main returns 0 if control reaches its end; if the body already returned,
       this extra Return is just never executed *)
    let instrs = block_conversion body in
    Tacky.Program (Tacky.Function (name, instrs @ [Tacky.Return (Tacky.Constant 0)]))

(* ---------- Pretty-printer ---------- *)
let pp_operator (op:Tacky.unary_operator):string=
  match op with
  |Tacky.Complement->"Complement"
  |Tacky.Negate->"Negate"
  |Tacky.Not->"Not"
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
  |Tacky.Equal->"Equal"
  |Tacky.NotEqual->"NotEqual"
  |Tacky.LessThan->"LessThan"
  |Tacky.LessOrEqual->"LessOrEqual"
  |Tacky.GreaterThan->"GreaterThan"
  |Tacky.GreaterOrEqual->"GreaterOrEqual"
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
  |Tacky.Copy (src,dst)->Printf.sprintf "Copy(%s, %s)" (pp_val src) (pp_val dst)
  |Tacky.Jump target->Printf.sprintf "Jump(%s)" target
  |Tacky.JumpIfZero (cond,target)->Printf.sprintf "JumpIfZero(%s, %s)" (pp_val cond) target
  |Tacky.JumpIfNotZero (cond,target)->Printf.sprintf "JumpIfNotZero(%s, %s)" (pp_val cond) target
  |Tacky.Label name->Printf.sprintf "Label(%s)" name
let pp_program (prog:Tacky.program):string=
  match prog with
  |Tacky.Program (Tacky.Function (_, instrs))->
    String.concat "\n" (List.map pp_instruction instrs)

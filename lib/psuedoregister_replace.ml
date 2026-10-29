let offsets = Hashtbl.create 16
let current = ref 0

let replace_operand (op:Assembly.operand):Assembly.operand=
  match op with
  |Assembly.Pseudo name->
    (match Hashtbl.find_opt offsets name with
     |Some offset-> Assembly.Stack offset
     |None->
       current := !current - 4;
       Hashtbl.add offsets name !current;
       Assembly.Stack !current)
  |other->other

let replace_instruction(is:Assembly.instruction):Assembly.instruction=
  match is with
  |Assembly.Mov (src,dst)->Assembly.Mov (replace_operand src, replace_operand dst)
  |Assembly.Unary(un,op)->Assembly.Unary(un,replace_operand op)
  |Assembly.Binary(bin,src,dst)->Assembly.Binary(bin,replace_operand src,replace_operand dst)
  |Assembly.Idiv op->Assembly.Idiv (replace_operand op)
  |Assembly.Cdq->Assembly.Cdq
  |Assembly.AllocateStack n->Assembly.AllocateStack n
  |Assembly.Ret->Assembly.Ret

let replace_function(f:Assembly.function_definition):Assembly.function_definition=
  match f with
  |Assembly.Function(name,instrs)->
    Assembly.Function (name, List.map replace_instruction instrs)
    
let replace_program(p:Assembly.program):Assembly.program*int=
  match p with
  |Assembly.Program f->
    let new_f = replace_function f in
    (Assembly.Program new_f, !current)
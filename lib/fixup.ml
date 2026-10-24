(* x86 can't mov memory to memory, so route those through R10 *)
let fix_instruction (is:Assembly.instruction):Assembly.instruction list=
  match is with
  |Assembly.Mov (Assembly.Stack src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.R10);
     Assembly.Mov (Assembly.Reg Assembly.R10, Assembly.Stack dst)]
  |other->[other]

(* stack_size is the last offset from Replace_pseudos, e.g. -8 *)
let fix_function (stack_size:int) (f:Assembly.function_definition):Assembly.function_definition=
  match f with
  |Assembly.Function (name, instrs)->
    Assembly.Function (name,
      Assembly.AllocateStack (-stack_size) :: List.concat_map fix_instruction instrs)

let fix_program (stack_size:int) (p:Assembly.program):Assembly.program=
  match p with
  |Assembly.Program f->Assembly.Program (fix_function stack_size f)

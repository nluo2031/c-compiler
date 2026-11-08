(* x86 can't mov memory to memory, so route those through R10 *)
let fix_instruction (is:Assembly.instruction):Assembly.instruction list=
  match is with
  |Assembly.Mov (Assembly.Stack src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.R10);
     Assembly.Mov (Assembly.Reg Assembly.R10, Assembly.Stack dst)]
  |Assembly.Idiv (Assembly.Imm n)->
    [Assembly.Mov (Assembly.Imm n, Assembly.Reg Assembly.R10);
     Assembly.Idiv (Assembly.Reg Assembly.R10)]
  |Assembly.Binary ((Assembly.Sal|Assembly.Sar) as op, Assembly.Stack src, dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.CX);
     Assembly.Binary (op, Assembly.Reg Assembly.CX, dst)]
  |Assembly.Binary ((Assembly.Add|Assembly.Sub|Assembly.And|Assembly.Or|Assembly.Xor) as op,
  Assembly.Stack src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.R10);
     Assembly.Binary (op, Assembly.Reg Assembly.R10, Assembly.Stack dst)]
  |Assembly.Binary (Assembly.Mult, src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack dst, Assembly.Reg Assembly.R11);
     Assembly.Binary (Assembly.Mult, src, Assembly.Reg Assembly.R11);
     Assembly.Mov (Assembly.Reg Assembly.R11, Assembly.Stack dst)]
  |Assembly.Cmp (Assembly.Stack a, Assembly.Stack b)->
    [Assembly.Mov (Assembly.Stack a, Assembly.Reg Assembly.R10);
     Assembly.Cmp (Assembly.Reg Assembly.R10, Assembly.Stack b)]
  |Assembly.Cmp (a, Assembly.Imm n)->
    [Assembly.Mov (Assembly.Imm n, Assembly.Reg Assembly.R11);
     Assembly.Cmp (a, Assembly.Reg Assembly.R11)]
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

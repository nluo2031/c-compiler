(* x86 can't mov memory to memory, so route those through R10 *)
let fix_instruction (is:Assembly.instruction):Assembly.instruction list=
  match is with
  |Assembly.Mov (Assembly.Stack src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.R10);
     Assembly.Mov (Assembly.Reg Assembly.R10, Assembly.Stack dst)]
  (* idivl can't take a constant operand *)
  |Assembly.Idiv (Assembly.Imm n)->
    [Assembly.Mov (Assembly.Imm n, Assembly.Reg Assembly.R10);
     Assembly.Idiv (Assembly.Reg Assembly.R10)]
  (* shifts take their count as a constant or in %cl, so load it into CX *)
  |Assembly.Binary ((Assembly.Sal|Assembly.Sar) as op, Assembly.Stack src, dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.CX);
     Assembly.Binary (op, Assembly.Reg Assembly.CX, dst)]
  (* add/sub/and/or/xor can't have two memory operands, so route the source through R10 *)
  |Assembly.Binary ((Assembly.Add|Assembly.Sub|Assembly.And|Assembly.Or|Assembly.Xor) as op,
                    Assembly.Stack src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack src, Assembly.Reg Assembly.R10);
     Assembly.Binary (op, Assembly.Reg Assembly.R10, Assembly.Stack dst)]
  (* imull can't write to memory, so multiply in R11 and store the result back *)
  |Assembly.Binary (Assembly.Mult, src, Assembly.Stack dst)->
    [Assembly.Mov (Assembly.Stack dst, Assembly.Reg Assembly.R11);
     Assembly.Binary (Assembly.Mult, src, Assembly.Reg Assembly.R11);
     Assembly.Mov (Assembly.Reg Assembly.R11, Assembly.Stack dst)]
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

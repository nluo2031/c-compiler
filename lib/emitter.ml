let show_label (macos : bool) (name : string) : string =
  if macos then "_" ^ name else name

let emit_operand (op : Assembly.operand) : string =
  match op with
  | Assembly.Imm n -> Printf.sprintf "$%d" n
  | Assembly.Reg Assembly.AX -> "%eax"
  | Assembly.Reg Assembly.DX -> "%edx"
  | Assembly.Reg Assembly.R10 -> "%r10d"
  | Assembly.Reg Assembly.R11 -> "%r11d"
  | Assembly.Stack n -> Printf.sprintf "%d(%%rbp)" n
  | Assembly.Pseudo name ->
      failwith ("pseudoregister " ^ name ^ " should have been replaced")

let emit_unop (op : Assembly.unary_operator) : string =
  match op with
  | Assembly.Neg -> "negl"
  | Assembly.Not -> "notl"

let emit_binop (op : Assembly.binary_operator) : string =
  match op with
  | Assembly.Add -> "addl"
  | Assembly.Sub -> "subl"
  | Assembly.Mult -> "imull"

let emit_instruction (i : Assembly.instruction) : string =
  match i with
  | Assembly.Mov (src, dst) ->
      Printf.sprintf "\tmovl\t%s, %s\n" (emit_operand src) (emit_operand dst)
  | Assembly.Unary (op, operand) ->
      Printf.sprintf "\t%s\t%s\n" (emit_unop op) (emit_operand operand)
  | Assembly.Binary (op, src, dst) ->
      Printf.sprintf "\t%s\t%s, %s\n" (emit_binop op) (emit_operand src) (emit_operand dst)
  | Assembly.Idiv operand -> Printf.sprintf "\tidivl\t%s\n" (emit_operand operand)
  | Assembly.Cdq -> "\tcdq\n"
  | Assembly.AllocateStack n -> Printf.sprintf "\tsubq\t$%d, %%rsp\n" n
  (* epilogue: tear down the stack frame before returning *)
  | Assembly.Ret -> "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n"

let emit_function (macos : bool) (f : Assembly.function_definition) : string =
  match f with
  | Assembly.Function (name, instructions) ->
      let label = show_label macos name in
      (* prologue: set up the stack frame so Stack offsets are relative to rbp *)
      Printf.sprintf "\t.globl %s\n%s:\n\tpushq\t%%rbp\n\tmovq\t%%rsp, %%rbp\n%s"
        label label
        (String.concat "" (List.map emit_instruction instructions))

let emit_program (macos : bool) (p : Assembly.program) : string =
  match p with
  | Assembly.Program f ->
      let footer =
        if macos then "" else "\t.section .note.GNU-stack,\"\",@progbits\n"
      in
      emit_function macos f ^ footer

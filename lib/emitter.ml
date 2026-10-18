let show_label (macos : bool) (name : string) : string =
  if macos then "_" ^ name else name

let emit_operand (op : Assembly.operand) : string =
  match op with
  | Assembly.Imm n -> Printf.sprintf "$%d" n
  | Assembly.Register -> "%eax"

let emit_instruction (i : Assembly.instruction) : string =
  match i with
  | Assembly.Mov (src, dst) ->
      Printf.sprintf "\tmovl\t%s, %s\n" (emit_operand src) (emit_operand dst)
  | Assembly.Ret -> "\tret\n"

let emit_function (macos : bool) (f : Assembly.function_definition) : string =
  match f with
  | Assembly.Function (name, instructions) ->
      let label = show_label macos name in
      Printf.sprintf "\t.globl %s\n%s:\n%s" label label
        (String.concat "" (List.map emit_instruction instructions))

let emit_program (macos : bool) (p : Assembly.program) : string =
  match p with
  | Assembly.Program f ->
      let footer =
        if macos then "" else "\t.section .note.GNU-stack,\"\",@progbits\n"
      in
      emit_function macos f ^ footer

let show_label (macos : bool) (name : string) : string =
  if macos then "_" ^ name else name

let emit_operand (op : Assembly.operand) : string =
  match op with
  | Assembly.Imm n -> Printf.sprintf "$%d" n
  | Assembly.Reg Assembly.AX -> "%eax"
  | Assembly.Reg Assembly.CX -> "%ecx"
  | Assembly.Reg Assembly.DX -> "%edx"
  | Assembly.Reg Assembly.R10 -> "%r10d"
  | Assembly.Reg Assembly.R11 -> "%r11d"
  | Assembly.Stack n -> Printf.sprintf "%d(%%rbp)" n
  | Assembly.Pseudo name ->
      failwith ("pseudoregister " ^ name ^ " should have been replaced")

(* setcc writes a single byte, so registers need their 1-byte names *)
let emit_byte_operand (op : Assembly.operand) : string =
  match op with
  | Assembly.Reg Assembly.AX -> "%al"
  | Assembly.Reg Assembly.CX -> "%cl"
  | Assembly.Reg Assembly.DX -> "%dl"
  | Assembly.Reg Assembly.R10 -> "%r10b"
  | Assembly.Reg Assembly.R11 -> "%r11b"
  | other -> emit_operand other

(* local labels need a platform prefix so they don't clash with function names *)
let local_label (macos : bool) (name : string) : string =
  if macos then "L" ^ name else ".L" ^ name

let cc_suffix (cc : Assembly.cond_code) : string =
  match cc with
  | Assembly.E -> "e"
  | Assembly.NE -> "ne"
  | Assembly.G -> "g"
  | Assembly.GE -> "ge"
  | Assembly.L -> "l"
  | Assembly.LE -> "le"

let emit_unop (op : Assembly.unary_operator) : string =
  match op with
  | Assembly.Neg -> "negl"
  | Assembly.Not -> "notl"

let emit_binop (op : Assembly.binary_operator) : string =
  match op with
  | Assembly.Add -> "addl"
  | Assembly.Sub -> "subl"
  | Assembly.Mult -> "imull"
  | Assembly.And -> "andl"
  | Assembly.Or -> "orl"
  | Assembly.Xor -> "xorl"
  | Assembly.Sal -> "sall"
  | Assembly.Sar -> "sarl"

let emit_instruction (macos : bool) (i : Assembly.instruction) : string =
  match i with
  | Assembly.Mov (src, dst) ->
      Printf.sprintf "\tmovl\t%s, %s\n" (emit_operand src) (emit_operand dst)
  | Assembly.Unary (op, operand) ->
      Printf.sprintf "\t%s\t%s\n" (emit_unop op) (emit_operand operand)
  (* a shift count held in a register must be the 1-byte %cl *)
  | Assembly.Binary ((Assembly.Sal | Assembly.Sar) as op, Assembly.Reg Assembly.CX, dst) ->
      Printf.sprintf "\t%s\t%%cl, %s\n" (emit_binop op) (emit_operand dst)
  | Assembly.Binary (op, src, dst) ->
      Printf.sprintf "\t%s\t%s, %s\n" (emit_binop op) (emit_operand src) (emit_operand dst)
  | Assembly.Idiv operand -> Printf.sprintf "\tidivl\t%s\n" (emit_operand operand)
  | Assembly.Cdq -> "\tcdq\n"
  | Assembly.AllocateStack n -> Printf.sprintf "\tsubq\t$%d, %%rsp\n" n
  (* epilogue: tear down the stack frame before returning *)
  | Assembly.Ret -> "\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n"
  | Assembly.Cmp (a, b) ->
      Printf.sprintf "\tcmpl\t%s, %s\n" (emit_operand a) (emit_operand b)
  | Assembly.Jmp target -> Printf.sprintf "\tjmp\t%s\n" (local_label macos target)
  | Assembly.JmpCC (cc, target) ->
      Printf.sprintf "\tj%s\t%s\n" (cc_suffix cc) (local_label macos target)
  | Assembly.SetCC (cc, operand) ->
      Printf.sprintf "\tset%s\t%s\n" (cc_suffix cc) (emit_byte_operand operand)
  | Assembly.Label name -> Printf.sprintf "%s:\n" (local_label macos name)

let emit_function (macos : bool) (f : Assembly.function_definition) : string =
  match f with
  | Assembly.Function (name, instructions) ->
      let label = show_label macos name in
      (* prologue: set up the stack frame so Stack offsets are relative to rbp *)
      Printf.sprintf "\t.globl %s\n%s:\n\tpushq\t%%rbp\n\tmovq\t%%rsp, %%rbp\n%s"
        label label
        (String.concat "" (List.map (emit_instruction macos) instructions))

let emit_program (macos : bool) (p : Assembly.program) : string =
  match p with
  | Assembly.Program f ->
      let footer =
        if macos then "" else "\t.section .note.GNU-stack,\"\",@progbits\n"
      in
      emit_function macos f ^ footer

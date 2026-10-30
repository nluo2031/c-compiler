type unary_operator =  Neg|Not
type binary_operator = Add|Sub|Mult|And|Or|Xor|Sal|Sar
type reg = AX|CX|DX|R10|R11
type cond_code = E|NE|G|GE|L|LE
type operand = Imm of int | Reg of reg |Pseudo of string|Stack of int
type instruction = Mov of operand * operand |Unary of unary_operator*operand
  | Binary of binary_operator*operand*operand | Idiv of operand | Cdq
  | AllocateStack of int| Ret
  | Cmp of operand*operand
  | Jmp of string
  | JmpCC of cond_code*string
  | SetCC of cond_code*operand
  | Label of string
type function_definition = Function of string * instruction list
type program = Program of function_definition

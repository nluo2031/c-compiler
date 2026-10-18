type operand = Imm of int | Register
type instruction = Mov of operand * operand | Ret
type function_definition = Function of string * instruction list
type program = Program of function_definition
type unary_operator =  Neg|Not
type reg = AX|R10
type operand = Imm of int | Reg of reg |Pseudo of string|Stack of int
type instruction = Mov of operand * operand |Unary of unary_operator*operand| AllocateStack of int| Ret
type function_definition = Function of string * instruction list
type program = Program of function_definition

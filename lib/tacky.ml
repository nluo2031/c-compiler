type unary_operator = Complement | Negate
type tacky_val = Constant of int | Var of string
type instruction = Return of tacky_val | Unary of unary_operator*tacky_val*tacky_val
type function_definition = Function of string*instruction list
type program = Program of function_definition

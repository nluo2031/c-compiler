type unary_operator = Complement | Negate
type exp = Constant of int | Unary of unary_operator*exp
type statement = Return of exp
type function_definition= Function of string * statement
type program = Program of function_definition

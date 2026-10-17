type exp = Constant of int
type statement = Return of exp
type function_definition= Function of string * statement
type program = Program of function_definition
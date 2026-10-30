type unary_operator = Complement | Negate | Not
type binary_operator = Add|Subtract|Multiply|Divide|Remainder
  |BitwiseAnd|BitwiseOr|BitwiseXor|LeftShift|RightShift
  |And|Or|Equal|NotEqual|LessThan|LessOrEqual|GreaterThan|GreaterOrEqual
type exp = Constant of int | Unary of unary_operator*exp|Binary of binary_operator*exp*exp
type statement = Return of exp
type function_definition= Function of string * statement
type program = Program of function_definition

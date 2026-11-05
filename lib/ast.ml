type unary_operator = Complement | Negate | Not
type binary_operator = Add|Subtract|Multiply|Divide|Remainder
  |BitwiseAnd|BitwiseOr|BitwiseXor|LeftShift|RightShift
  |And|Or|Equal|NotEqual|LessThan|LessOrEqual|GreaterThan|GreaterOrEqual
type exp = Constant of int | Var of string | Unary of unary_operator*exp|Binary of binary_operator*exp*exp
  |Assignment of exp*exp
  |Conditional of exp*exp*exp
type statement = Return of exp | Expression of exp | If of exp*statement*statement option | Null
type declaration = Declaration of string * exp option
type block_item = S of statement | D of declaration
type function_definition= Function of string * block_item list
type program = Program of function_definition

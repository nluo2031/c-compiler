type unary_operator = Complement | Negate | Not
type binary_operator = Add | Subtract | Multiply | Divide | Remainder
  | BitwiseAnd | BitwiseOr | BitwiseXor | LeftShift | RightShift
  | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual
type tacky_val = Constant of int | Var of string
type instruction = Return of tacky_val | Unary of unary_operator*tacky_val*tacky_val
  | Binary of binary_operator*tacky_val*tacky_val*tacky_val|Copy of tacky_val*tacky_val
  | Jump of string
  | JumpIfZero of tacky_val*string
  | JumpIfNotZero of tacky_val*string
  | Label of string
type function_definition = Function of string*instruction list
type program = Program of function_definition

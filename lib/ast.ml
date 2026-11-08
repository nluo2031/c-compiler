type unary_operator = Complement | Negate | Not
type binary_operator = Add|Subtract|Multiply|Divide|Remainder
  |BitwiseAnd|BitwiseOr|BitwiseXor|LeftShift|RightShift
  |And|Or|Equal|NotEqual|LessThan|LessOrEqual|GreaterThan|GreaterOrEqual
type exp = Constant of int | Var of string | Unary of unary_operator*exp|Binary of binary_operator*exp*exp
  |Assignment of exp*exp
  |CompoundAssignment of binary_operator*exp*exp (* x += e is CompoundAssignment (Add, x, e) *)
  |Conditional of exp*exp*exp
(* Loop labels: the parser leaves them as "" and semantic analysis fills them in,
   so each break/continue carries the label of the loop it belongs to. *)
type statement = Return of exp | Expression of exp | If of exp*statement*statement option
  |Compound of block
  |Break of string
  |Continue of string
  |While of exp*statement*string
  |DoWhile of statement*exp*string
  |For of for_init*exp option*exp option*statement*string
  |Null
and declaration = Declaration of string * exp option
and for_init = InitDecl of declaration | InitExp of exp option
and block_item = S of statement | D of declaration
and block = Block of block_item list
type function_definition= Function of string * block
type program = Program of function_definition

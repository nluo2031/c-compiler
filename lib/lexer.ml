type token = 
  | Identifier of string (* e.g. main, foo, my_var1 *)
  | Constant of int (* e.g. 2, 100 *)
  | KwInt (* int *)
  | KwVoid (* void *)
  | KwReturn (* return *)
  | OpenParen (* ( *)
  | CloseParen (* ) *)
  | OpenBrace (* { *)
  | CloseBrace (* } *)
  | Semicolon (* ; *)

exception Lex_error of string

let string_of_token = function
  | Identifier name -> Printf.sprintf "Identifier(%s)" name
  | Constant n -> Printf.sprintf "Constant(%d)" n
  | KwInt -> "int"
  | KwVoid -> "void"
  | KwReturn -> "return"
  | OpenParen -> "("
  | CloseParen -> ")"
  | OpenBrace -> "{"
  | CloseBrace -> "}"
  | Semicolon -> ";"

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let is_ident_start c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_ident_char c = is_ident_start c || is_digit c

let is_whitespace c =
  match c with
  | ' ' | '\t' | '\n' | '\r' | '\012' | '\011' -> true
  | _ -> false

let rec scan_while (f : char->bool) (s:string) (i:int):int=
  if i<String.length s && f s.[i]
    then scan_while f s (i+1)
else i

let keyword_or_identifier (word:string):token=
  match word with
  | "int" -> KwInt
  | "void" -> KwVoid
  | "return" -> KwReturn
  | _ -> Identifier word

let lex (s : string) : token list =
  let len = String.length s in
  let rec go (i:int)(acc:token list) :token list=
    if i>=len then
      List.rev acc
    else
      match s.[i] with 
      | c when is_whitespace c -> go (i + 1) acc
      | '(' -> go (i + 1) (OpenParen :: acc)
      | ')' -> go (i + 1) (CloseParen :: acc)
      | '{' -> go (i + 1) (OpenBrace :: acc)
      | '}' -> go (i + 1) (CloseBrace :: acc)
      | ';' -> go (i + 1) (Semicolon :: acc)
      | c when is_ident_start c ->
          let j = scan_while is_ident_char s i in
          let word = String.sub s i (j - i) in
          go j (keyword_or_identifier word :: acc)
      | c when is_digit c ->
          let j = scan_while is_digit s i in
          if j < len && is_ident_char s.[j] then
            raise
              (Lex_error
                 (Printf.sprintf "invalid constant at position %d: %s" i
                    (String.sub s i (scan_while is_ident_char s i - i))))
          else
            let text = String.sub s i (j - i) in
            go j (Constant (int_of_string text) :: acc)
      | c ->
          raise
            (Lex_error
               (Printf.sprintf "unexpected character '%c' at position %d" c i))
  in
  go 0 []

let lex_file (path : string) : token list =
  let contents = In_channel.with_open_bin path In_channel.input_all in
  lex contents
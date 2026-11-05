(* Semantic analysis: checks that variables are declared exactly once before
   use and that assignments target lvalues, and renames each variable to a
   globally unique name for TACKY generation. *)

exception Semantic_error of string

module StringMap = Map.Make (String)

let counter = ref 0

(* Identifiers can't contain dots, so these never clash with a source name,
   and the ".var." keeps them distinct from TACKY's "tmp.N" temporaries. *)
let make_unique (name : string) : string =
  incr counter;
  name ^ ".var." ^ string_of_int !counter

let rec resolve_exp (vars : string StringMap.t) (e : Ast.exp) : Ast.exp =
  match e with
  | Ast.Constant _ -> e
  | Ast.Var name -> (
      match StringMap.find_opt name vars with
      | Some unique -> Ast.Var unique
      | None ->
          raise
            (Semantic_error (Printf.sprintf "undeclared variable %s" name)))
  | Ast.Unary (op, inner) -> Ast.Unary (op, resolve_exp vars inner)
  | Ast.Binary (op, left, right) ->
      Ast.Binary (op, resolve_exp vars left, resolve_exp vars right)
  | Ast.Assignment ((Ast.Var _ as left), right) ->
      Ast.Assignment (resolve_exp vars left, resolve_exp vars right)
  | Ast.Assignment _ ->
      raise (Semantic_error "invalid lvalue on left side of assignment")
  | Ast.Conditional (condition, e1, e2) ->
      Ast.Conditional
        (resolve_exp vars condition, resolve_exp vars e1, resolve_exp vars e2)

let rec resolve_statement (vars : string StringMap.t) (s : Ast.statement) :
    Ast.statement =
  match s with
  | Ast.Return e -> Ast.Return (resolve_exp vars e)
  | Ast.Expression e -> Ast.Expression (resolve_exp vars e)
  | Ast.If (condition, then_branch, else_branch) ->
      Ast.If
        ( resolve_exp vars condition,
          resolve_statement vars then_branch,
          Option.map (resolve_statement vars) else_branch )
  | Ast.Null -> Ast.Null

let resolve_declaration (vars : string StringMap.t) (d : Ast.declaration) :
    string StringMap.t * Ast.declaration =
  match d with
  | Ast.Declaration (name, init) ->
      if StringMap.mem name vars then
        raise
          (Semantic_error (Printf.sprintf "duplicate declaration of %s" name));
      let unique = make_unique name in
      (* the name is in scope in its own initializer, as in: int a = a = 5; *)
      let vars = StringMap.add name unique vars in
      (vars, Ast.Declaration (unique, Option.map (resolve_exp vars) init))

let resolve_block_item (vars : string StringMap.t) (item : Ast.block_item) :
    string StringMap.t * Ast.block_item =
  match item with
  | Ast.S s -> (vars, Ast.S (resolve_statement vars s))
  | Ast.D d ->
      let vars, d = resolve_declaration vars d in
      (vars, Ast.D d)

let resolve_program (p : Ast.program) : Ast.program =
  match p with
  | Ast.Program (Ast.Function (name, body)) ->
      let _, body = List.fold_left_map resolve_block_item StringMap.empty body in
      Ast.Program (Ast.Function (name, body))

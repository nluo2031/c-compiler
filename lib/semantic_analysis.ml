(* Semantic analysis, in two passes:
   1. Variable resolution: checks that variables are declared exactly once per
      scope before use and that assignments target lvalues, and renames each
      variable to a globally unique name for TACKY generation.
   2. Loop labeling: gives every loop a unique label and tags each break and
      continue with the label of its innermost enclosing loop. *)

exception Semantic_error of string

module StringMap = Map.Make (String)

let counter = ref 0

(* Identifiers can't contain dots, so these never clash with a source name,
   and the ".var." keeps them distinct from TACKY's "tmp.N" temporaries. *)
let make_unique (name : string) : string =
  incr counter;
  name ^ ".var." ^ string_of_int !counter

(* ---------- Variable resolution ---------- *)

(* from_current_block distinguishes a redeclaration in the same block (an
   error) from one in an inner block (legal shadowing). *)
type var_entry = { unique_name : string; from_current_block : bool }

type var_map = var_entry StringMap.t

(* entering a new block: everything visible so far now comes from an outer block *)
let copy_variable_map (vars : var_map) : var_map =
  StringMap.map (fun entry -> { entry with from_current_block = false }) vars

let rec resolve_exp (vars : var_map) (e : Ast.exp) : Ast.exp =
  match e with
  | Ast.Constant _ -> e
  | Ast.Var name -> (
      match StringMap.find_opt name vars with
      | Some entry -> Ast.Var entry.unique_name
      | None ->
          raise
            (Semantic_error (Printf.sprintf "undeclared variable %s" name)))
  | Ast.Unary (op, inner) -> Ast.Unary (op, resolve_exp vars inner)
  | Ast.Binary (op, left, right) ->
      Ast.Binary (op, resolve_exp vars left, resolve_exp vars right)
  | Ast.Assignment ((Ast.Var _ as left), right) ->
      Ast.Assignment (resolve_exp vars left, resolve_exp vars right)
  | Ast.CompoundAssignment (op, (Ast.Var _ as left), right) ->
      Ast.CompoundAssignment (op, resolve_exp vars left, resolve_exp vars right)
  | Ast.Assignment _ | Ast.CompoundAssignment _ ->
      raise (Semantic_error "invalid lvalue on left side of assignment")
  | Ast.Conditional (condition, e1, e2) ->
      Ast.Conditional
        (resolve_exp vars condition, resolve_exp vars e1, resolve_exp vars e2)

let resolve_opt_exp (vars : var_map) (e : Ast.exp option) : Ast.exp option =
  Option.map (resolve_exp vars) e

let resolve_declaration (vars : var_map) (d : Ast.declaration) :
    var_map * Ast.declaration =
  match d with
  | Ast.Declaration (name, init) ->
      (match StringMap.find_opt name vars with
      | Some { from_current_block = true; _ } ->
          raise
            (Semantic_error (Printf.sprintf "duplicate declaration of %s" name))
      | _ -> ());
      let unique = make_unique name in
      (* the name is in scope in its own initializer, as in: int a = a = 5; *)
      let vars =
        StringMap.add name { unique_name = unique; from_current_block = true } vars
      in
      (vars, Ast.Declaration (unique, resolve_opt_exp vars init))

let resolve_for_init (vars : var_map) (init : Ast.for_init) : var_map * Ast.for_init =
  match init with
  | Ast.InitDecl d ->
      let vars, d = resolve_declaration vars d in
      (vars, Ast.InitDecl d)
  | Ast.InitExp e -> (vars, Ast.InitExp (resolve_opt_exp vars e))

let rec resolve_statement (vars : var_map) (s : Ast.statement) : Ast.statement =
  match s with
  | Ast.Return e -> Ast.Return (resolve_exp vars e)
  | Ast.Expression e -> Ast.Expression (resolve_exp vars e)
  | Ast.If (condition, then_branch, else_branch) ->
      Ast.If
        ( resolve_exp vars condition,
          resolve_statement vars then_branch,
          Option.map (resolve_statement vars) else_branch )
  | Ast.Compound b -> Ast.Compound (resolve_block (copy_variable_map vars) b)
  | Ast.While (condition, body, label) ->
      Ast.While (resolve_exp vars condition, resolve_statement vars body, label)
  | Ast.DoWhile (body, condition, label) ->
      Ast.DoWhile (resolve_statement vars body, resolve_exp vars condition, label)
  | Ast.For (init, condition, post, body, label) ->
      (* the for header opens its own scope, so int i = 0 there can shadow an outer i *)
      let vars = copy_variable_map vars in
      let vars, init = resolve_for_init vars init in
      Ast.For
        ( init,
          resolve_opt_exp vars condition,
          resolve_opt_exp vars post,
          resolve_statement vars body,
          label )
  | Ast.Break _ | Ast.Continue _ | Ast.Null -> s

and resolve_block_item (vars : var_map) (item : Ast.block_item) :
    var_map * Ast.block_item =
  match item with
  | Ast.S s -> (vars, Ast.S (resolve_statement vars s))
  | Ast.D d ->
      let vars, d = resolve_declaration vars d in
      (vars, Ast.D d)

(* the caller decides whether this block gets a fresh scope *)
and resolve_block (vars : var_map) (b : Ast.block) : Ast.block =
  match b with
  | Ast.Block items ->
      let _, items = List.fold_left_map resolve_block_item vars items in
      Ast.Block items

(* ---------- Loop labeling ---------- *)

let make_loop_label () : string =
  incr counter;
  "loop." ^ string_of_int !counter

(* current is the label of the innermost enclosing loop, if any *)
let rec label_statement (current : string option) (s : Ast.statement) : Ast.statement =
  match s with
  | Ast.Break _ -> (
      match current with
      | Some label -> Ast.Break label
      | None -> raise (Semantic_error "break statement outside of loop"))
  | Ast.Continue _ -> (
      match current with
      | Some label -> Ast.Continue label
      | None -> raise (Semantic_error "continue statement outside of loop"))
  | Ast.While (condition, body, _) ->
      let label = make_loop_label () in
      Ast.While (condition, label_statement (Some label) body, label)
  | Ast.DoWhile (body, condition, _) ->
      let label = make_loop_label () in
      Ast.DoWhile (label_statement (Some label) body, condition, label)
  | Ast.For (init, condition, post, body, _) ->
      let label = make_loop_label () in
      Ast.For (init, condition, post, label_statement (Some label) body, label)
  | Ast.If (condition, then_branch, else_branch) ->
      Ast.If
        ( condition,
          label_statement current then_branch,
          Option.map (label_statement current) else_branch )
  | Ast.Compound b -> Ast.Compound (label_block current b)
  | Ast.Return _ | Ast.Expression _ | Ast.Null -> s

and label_block (current : string option) (b : Ast.block) : Ast.block =
  match b with
  | Ast.Block items ->
      Ast.Block
        (List.map
           (function
             | Ast.S s -> Ast.S (label_statement current s)
             | Ast.D _ as d -> d)
           items)

let resolve_program (p : Ast.program) : Ast.program =
  match p with
  | Ast.Program (Ast.Function (name, body)) ->
      let body = resolve_block StringMap.empty body in
      let body = label_block None body in
      Ast.Program (Ast.Function (name, body))

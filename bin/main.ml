(* Compiler driver.

   Usage: main.exe [--lex | --parse | --codegen | -S] path/to/file.c

   Steps:
     1. Preprocess: gcc -E -P file.c -o file.i
     2. Compile file.i -> file.s   (only lexing is implemented so far)
     3. Assemble + link: gcc file.s -o file

   --lex stops after lexing and writes no output files. *)

open Compiler

type stage = Lex | Parse | Codegen | Assembly | Full

let usage () =
  prerr_endline "usage: main.exe [--lex | --parse | --codegen | -S] <file.c>";
  exit 2

let parse_args () =
  let stage = ref Full and file = ref None in
  Array.iteri
    (fun i arg ->
      if i > 0 then
        match arg with
        | "--lex" -> stage := Lex
        | "--parse" -> stage := Parse
        | "--codegen" -> stage := Codegen
        | "-S" -> stage := Assembly
        | s when String.length s > 0 && s.[0] = '-' ->
            Printf.eprintf "unknown option: %s\n" s;
            usage ()
        | s -> file := Some s)
    Sys.argv;
  match !file with Some f -> (!stage, f) | None -> usage ()

let run_cmd cmd =
  let code = Sys.command cmd in
  if code <> 0 then (
    Printf.eprintf "command failed (exit %d): %s\n" code cmd;
    exit 1)

let () =
  let stage, src = parse_args () in
  let base = Filename.remove_extension src in
  let preprocessed = base ^ ".i" in
  run_cmd
    (Printf.sprintf "gcc -E -P %s -o %s" (Filename.quote src)
       (Filename.quote preprocessed));
  let tokens =
    match Lexer.lex_file preprocessed with
    | tokens -> Sys.remove preprocessed; tokens
    | exception Lexer.Lex_error msg ->
        Sys.remove preprocessed;
        Printf.eprintf "lexer error: %s\n" msg;
        exit 1
  in
  match stage with
  | Lex ->
      List.iter (fun t -> print_endline (Lexer.string_of_token t)) tokens
  | Parse | Codegen | Assembly | Full ->
      prerr_endline "error: only lexing (--lex) is implemented so far";
      exit 1

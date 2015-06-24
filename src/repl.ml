open Ast
open Interpreter
open Printf

let rec fmt = function
  | List l -> fmt_list l
  | Operation o ->
    begin
      match o with
      | Prefix (op, ast) -> fmt_op op ^ fmt ast
      | Infix (op, left, right) -> fmt left ^ fmt_op op ^ fmt right
      | ReduceOp (op, ast) -> fmt_op op ^ "/ " ^ fmt ast
    end
and fmt_list = function
  | [] -> ""
  | a :: t -> string_of_float a ^ " " ^ fmt_list t
and fmt_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "x"
  | Divide -> "÷"
  | Reduce -> "/"

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf |> interpret |> fmt in
      printf "%s\n" result;
      flush stdout;
      Lexing.flush_input lexbuf
    done
  with ex -> (
    match ex with
    | Lexer.Eof -> ()
    | Lexer.Unknown s -> printf "%s\n" s |> (fun _ -> flush stdout)
    | _ -> ();
    exit 0
  )


{
  open Parser

  exception Eof
  exception Unknown of string
}

rule token = parse
  | [' ' '\t' '\r']
    { token lexbuf }
  | ['\n']
    { EOL }
  | '-'? ['0'-'9']+ '.'? ['0'-'9']* as f
    { FLOAT (float_of_string f) }
  | '-'? '.' ['0'-'9']+ as f
    { FLOAT (float_of_string f) }
  | "➕"
    { PLUS }
  | "➖"
    { MINUS }
  | "✖"
    { TIMES }
  | "➗"
    { DIVIDE }
  | "📥"
    { REDUCE }
  | eof 
    { raise Eof }
  | _
    { raise (Unknown (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

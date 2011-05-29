{
  open Crapparser
}

let digit = ['0'-'9']
let sign = ['-' '+']
let id = ['a'-'z'] ['a'-'z' '0'-'9' '-' '_' '\'']*

rule token = parse
  | ';'[^'\n']*'\n'     { token lexbuf }
  | [' ' '\t']+    { SEPARATOR }
  | '\n'+          { NEWLINE }
  | sign? digit+
  | sign? "." digit+
  | sign? digit+ "." digit* as num
                  { NUM (float_of_string num) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MULTIPLY }
  | '/'           { DIVIDE }
  | ".+"          { P_PLUS }
  | ".-"          { P_MINUS }
  | ".*"          { P_SCALE_UP }
  | "./"          { P_SCALE_DOWN }
  | ".!"          { P_CREATE }
  | "..."         { PRINT_STACK }
  | ".v."         { PRINT_DICTIONARY }
  | "->"          { ASSIGN }
  | id as name    { ID (name) }
  | '['           { LOOP_START }
  | ']'           { LOOP_END }
  | eof           { raise End_of_file }

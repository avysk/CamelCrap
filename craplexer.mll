{
  open Crapparser
}

let digit = ['0'-'9']
let sign = ['-' '+']
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' '0'-'9' '-' '_' '\'']*

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
  | '['           { LOOP_START }
  | ']'           { LOOP_END }
  | '<'           { LT }
  | '>'           { GT }
  | "line"        { LINE }
  | "circle"      { CIRCLE }
  | "ngonloop"    { NGONLOOP }
  | "makeCircle"  { MAKE_CIRCLE }
  | "makeEllipse" { MAKE_ELLIPSE }
  | "makeNgon"    { MAKE_NGON }
  | id as name    { ID (name) }
  | eof           { raise End_of_file }

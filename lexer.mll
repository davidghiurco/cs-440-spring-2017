(* File lexer.mll *)
{
  open Parser
  exception Eof
  exception Syntax_error of string
  let line_num = ref 1
  let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))
  let keywords = [
  "pokemon", POKEMON;
  "fight", FIGHT;
  "fire", FIRE;
  "water", WATER;
  "electric", ELECTRIC;
  "grass", GRASS;
  ]
}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let digits = digit+
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule token = parse
blank             { token lexbuf }     (* skip blanks *)
| ['\n' ]         { incr line_num ; token lexbuf } (* after a line break increment total number of lines *)
| ";"             { EOL }
| digits as d   { INT(int_of_string d) }
| '+'             { PLUS }
| _               { syntax_error "couldn't identify the token" }
| eof             { raise Eof }
| iden as i       { (* try keywords if not found then it's identifier *)
let l = String.lowercase i
in try List.assoc l keywords
with Not_found -> IDENTIFIER i }

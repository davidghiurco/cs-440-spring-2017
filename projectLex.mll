(* File projectLex.mll *)
{
	open Parser        (* The type token is defined in parser.mli *)
    exception Eof
}
(* current token line number *)
let line_num = ref 1

exception Syntax_error of string

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

let keyword_table = 
    ["pokemon", POKEMON;
	"fight", FIGHT;
    "fire", FIRE;
	"water", WATER;
	"electric", ELECTRIC;
	"grass", GRASS;
	]


let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let digits = digit*
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule token = parse
	[' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | digits as d {
      (* parse literal *)
      LITERAL (int_of_string d) }
  | '\n'     { incr line_num; micro lexbuf } (* counting new line characters *)
  | blank    { micro lexbuf } (* skipping blank characters *)
  | _        { syntax_error "couldn't identify the token" }
  | eof      { EOF } (* no more tokens *)


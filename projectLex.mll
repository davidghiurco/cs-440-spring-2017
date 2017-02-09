(* File projectLex.mll *)
{
	open Parser        (* The type token is defined in parser.mli *)
    exception Eof
}
{ let keyword_table = Hashtbl.create 53
	let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    ["pokemon", POKEMON;
		"fight", FIGHT;
        "fire", FIRE;
		"water", WATER;
		"electric", ELECTRIC;
		"grass", GRASS;
	]
}
rule token = parse
	[' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
	{ try
		Hashtbl.find keyword_table id
      with Not_found ->
        IDENT id }
  | eof            { raise Eof }
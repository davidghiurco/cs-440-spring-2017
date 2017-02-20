/* File parser.mly */
%{
  (*
  let vars = Hashtbl.create 100

  let is_alloc_var v = Hashtbl.mem !vars v

  let alloc_var v = if is_alloc_var v then var v
                    else let _ = Hashtbl.replace !vars v (empty_var 1) in var v

  let generate_assign a b = generate_copy (alloc_var a) b*)
%}

%token <int> INT
%token PLUS
%token POKEMON FIGHT FIRE WATER ELECTRIC GRASS
%token <string> ASSIGN
%token <string> IDENTIFIER
%token EOL
%start main             /* the entry point */
%type <int> main
%%
main:
expr EOL                { $1 }
;
expr:
INT                     { $1 }
| expr PLUS expr            { print_string ">> "; $1 + $3 }
| pokemon_type INT FIGHT FIRE INT   { print_string ">> "; if $2 > $5 then $2 else $5 }
;

pokemon_type:
| FIRE {}
| GRASS {}
| ELECTRIC {}
| WATER {}

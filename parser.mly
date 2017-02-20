/* File parser.mly */
%{

let is_larger v1 v2 = if v1 > v2 then v1 else v2;;
exception Codegen_error of string
let codegen_error msg = raise (Codegen_error msg)

let pokemon_fight p1 v1 p2 v2 =
  if p1 = "FIRE" then
    if p2 = "FIRE" then is_larger v1 v2
    else if p2 = "GRASS" then let v3 = 2*v1 in is_larger v3 v2;
    else if p2 = "ELECTRIC" then is_larger v1 v2
    else if p2 = "WATER" then is_larger v1 2*v2
    else codegen_error "Invalid Pokemon Type"
  else if p1 = "GRASS" then
      if p2 = "FIRE" then is_larger v1 2*v2
      else if p2 = "GRASS" then is_larger v1 v2
      else if p2 = "ELECTRIC" then is_larger v1 v2
      else if p2 = "WATER" then let v3 = 2*v1 in is_larger v3 v2
      else codegen_error "Invalid Pokemon Type"
  else if p1 = "ELECTRIC" then
      if p2 = "FIRE" then is_larger v1 v2
      else if p2 = "GRASS" then is_larger v1 2*v2
      else if p2 = "ELECTRIC" then is_larger v1 v2
      else if p2 = "WATER" then let v3 = 2*v1 in is_larger v3 v2
      else codegen_error "Invalid Pokemon Type"
  else if p1 = "WATER" then
      if p2 = "FIRE" then let v3 = 2*v1 in is_larger v3 v2
      else if p2 = "GRASS" then is_larger v1 v2
      else if p2 = "ELECTRIC" then is_larger v1 2*v2
      else if p2 = "WATER" then is_larger v1 v2
      else codegen_error "Invalid Pokemon Type"
  else
    codegen_error "Invalid Pokemon Type";;

let is_valid_value v = if v > 0 then v else codegen_error "Pokeon value is not valid. Must be positive integer.";;
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
expr EOL                                    { $1 }
;
expr:
INT                                         { is_valid_value $1 }
| expr PLUS expr                            { print_string ">> "; $1 + $3 }
| pokemon_type INT FIGHT pokemon_type INT   { print_string ">> "; pokemon_fight $1 $2 $4 $5 }
;

pokemon_type:
| FIRE {"FIRE"}
| GRASS {"GRASS"}
| ELECTRIC {"ELECTRIC"}
| WATER {"WATER"}

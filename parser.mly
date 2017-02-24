/* File parser.mly */
%{

  let pokemon_fight_results winner loser damage : int=
    match damage with
    | 0 -> Printf.printf "%s\n" "Tie" ; damage;
    | x -> Printf.printf "%s beats %s by %d damage\n" winner loser x; damage;
  ;;

  let pokemon_fight_helper p1 v1 p2 v2 : int =
  if v1 = v2 then pokemon_fight_results p1 p2 0
  else if v1 > v2 then let damage=v1-v2 in pokemon_fight_results p1 p2 damage
  else let damage=v2-v1 in pokemon_fight_results p2 p1 damage;;

  exception Codegen_error of string
  let codegen_error msg = raise (Codegen_error msg)

  let pokemon_fight (p1: string) (v1 : int) (p2 : string) (v2 : int) : int =
  if p1 = "FIRE" then
  if p2 = "FIRE" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "GRASS" then let v3 = 2*v1 in pokemon_fight_helper p1 v3 p2 v2
  else if p2 = "ELECTRIC" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "WATER" then let v3 = 2*v2 in pokemon_fight_helper p1 v1 p2 v3
  else codegen_error "Invalid Pokemon Type"
  else if p1 = "GRASS" then
  if p2 = "FIRE" then let v3 = 2*v2 in pokemon_fight_helper p1 v1 p2 v3
  else if p2 = "GRASS" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "ELECTRIC" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "WATER" then let v3 = 2*v1 in pokemon_fight_helper p1 v3 p2 v2
  else codegen_error "Invalid Pokemon Type"
  else if p1 = "ELECTRIC" then
  if p2 = "FIRE" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "GRASS" then let v3 = 2*v2 in pokemon_fight_helper p1 v1 p2 v3
  else if p2 = "ELECTRIC" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "WATER" then let v3 = 2*v1 in pokemon_fight_helper p1 v3 p2 v2
  else codegen_error "Invalid Pokemon Type"
  else if p1 = "WATER" then
  if p2 = "FIRE" then let v3 = 2*v1 in pokemon_fight_helper p1 v3 p2 v2
  else if p2 = "GRASS" then pokemon_fight_helper p1 v1 p2 v2
  else if p2 = "ELECTRIC" then let v3 = 2*v2 in pokemon_fight_helper p1 v1 p2 v3
  else if p2 = "WATER" then pokemon_fight_helper p1 v1 p2 v2
  else codegen_error "Invalid Pokemon Type"
  else
  codegen_error "Invalid Pokemon Type";;

  let is_valid_value v = if v > 0 then v else codegen_error "Pokemon value is not valid. Must be positive integer.";;
  %}

  %token <int> INT
  %token PLUS ASSIGN
  %token POKEMON FIGHT FIRE WATER ELECTRIC GRASS
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
  | pokemon_type INT FIGHT pokemon_type INT   { print_string ">> " ; pokemon_fight $1 $2 $4 $5 }
  | pokemon_type IDENTIFIER ASSIGN expr       { Printf.printf "%s %s \n" $1 $2 ; $4 }
  ;

  pokemon_type:
  | FIRE {"FIRE"}
  | GRASS {"GRASS"}
  | ELECTRIC {"ELECTRIC"}
  | WATER {"WATER"}

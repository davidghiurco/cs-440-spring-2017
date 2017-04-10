/* File parser.mly */
%{

  type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree;;
  type ast_node = {data_type: string; value: string; token: string };;

  let draw tree =
    let rec print indent tree =
      match tree with
          Leaf n ->
          Printf.printf "%s%s--%s\n" indent n.token n.value
       | Node (left, n, right) ->
          Printf.printf "%s------\n" indent;
          print (indent ^ "| ") left;
          Printf.printf "%s%s\n" indent n.token;
          print (indent ^ "| ") right;
          Printf.printf "%s------\n" indent
    in
    print "" tree
    ;;

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
  type symbols_table_entry = {pokemontype:string; power: int};;
  let is_valid_value v = if v > 0 then v else codegen_error "Pokemon value is not valid. Must be positive integer.";;
  let symbols_table = ref (Hashtbl.create 12345);;
  let assign_var (key: string) (poke_type: string) (p: int) = let p_entry = {pokemontype=poke_type;power=p} in Hashtbl.replace !symbols_table key p_entry ;;
  let get_var key = Hashtbl.find !symbols_table key ;;

  let iden_fight_iden (i1: string) (i2: string) =
    let p1 = get_var i1 in
    let p2 = get_var i2 in
    pokemon_fight p1.pokemontype p1.power p2.pokemontype p2.power;;
  %}

  %token <int> INT
  %token PLUS ASSIGN
  %token POKEMON FIGHT FIRE WATER ELECTRIC GRASS
  %token <string> IDENTIFIER
  %token <string> STRING
  %token EOL
  %start main             /* the entry point */
  %type <int> main
  %%
  main:
  Statements EOL                                { let t = Node(
                                                    $1,
                                                    {data_type="int";value="MAIN";token="MAIN"},
                                                    Leaf {data_type="string";value=";";token="EOL"}) ;
                                                  print_string "---\n";
                                                  draw t; 1}
  ;

  Statements:
  | Statement                                   { $1 }
  ;

  Statement:
  | Assignment                                  { $1 }
  | FIGHT                                       { $1 } (* do calculation here *)
  ;

  Assignment:
  | Declaration ASSIGN Literal                       { }
  ;

  Declaration:
  | Pokemon_Type IDENTIFIER         { }
  ;

  Literal:
  | INT                                         {Leaf {data_type="int"; value=$1; token="LITERAL"}}
  | STRING                                      {Leaf {data_type="string"; value=$1; token="LITERAL"}}
  ;

  Pokemon_Type:
  | FIRE        {Leaf {data_type="FIRE";value="FIRE";token="Pokemon_Type"}}
  | GRASS       {Leaf {data_type="GRASS";value="GRASS";token="Pokemon_Type"}}
  | ELECTRIC    {Leaf {data_type="ELECTRIC";value="ELECTRIC";token="Pokemon_Type"}}
  | WATER       {Leaf {data_type="WATER";value="WATER";token="Pokemon_Type"}}

  Fight:
  | Fight FIGHT IDENTIFIER               { Node(
                                              $1 ,
                                              {data_type="string"; value="fight"; token="FIGHT"},
                                              Leaf {data_type="string"; value=$1; token="IDENTIFIER"}
                                              );}

  | Pokemon_Type Literal FIGHT Fight     { $1.value = $2; Node(
                                              $1 ,
                                              {data_type="string"; value="fight"; token="FIGHT"},
                                              Leaf {data_type="string"; value=$1; token="IDENTIFIER"}
                                              ); }

  | IDENTIFIER                                      {Leaf {data_type="string"; value=$1; token="IDENTIFIER"}}  (* check here if iden exists in the hastbl and set the value else error *)
  | Pokemon_Type Literal                            {Leaf {data_type="Pokemon_Type"; value=$2; token="Pokemon_Type"}} (* type check here that its an int not a string * )
  ;

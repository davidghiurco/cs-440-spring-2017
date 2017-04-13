/* File parser.mly */
%{

  type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree | Node_OneChild of 'a tree * 'a;;
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
  | Node_OneChild (left, n) ->
  Printf.printf "%s------\n" indent;
  print (indent ^ "| ") left;
  Printf.printf "%s%s\n" indent n.token;
  Printf.printf "%s------\n" indent
  in
  print "" tree
  ;;

  let pokemon_fight_results winner loser damage =
  match damage with
  | 0 -> Printf.printf "%s\n" "Tie";
  | x -> Printf.printf "%s beats %s by %d damage\n" winner loser x;
  ;;

  let pokemon_fight_helper p1 v1 p2 v2 : int =
  if v1 = v2 then begin
    pokemon_fight_results p1 p2 0;
    1; (* ties go to the first pokemon *)
  end
  else if v1 > v2 then begin
    let damage=v1-v2 in pokemon_fight_results p1 p2 damage;
    1;
  end
  else begin
    let damage=v2-v1 in pokemon_fight_results p2 p1 damage;
    2;
  end
  ;;

  let get_type_from_leaf_node n: string =
  match n with
  Leaf n -> n.data_type;;

  exception Codegen_error of string
  let codegen_error msg = raise (Codegen_error msg);;

  exception Typecheck_error of string
  let typecheck_error emsg = raise (Typecheck_error emsg);;

  exception Name_error of string
  let name_error iden = raise (Name_error (iden^" is not defined"));;

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

  let get_stored_pokemon_type (iden : string) : string =
  let p = Hashtbl.find !symbols_table iden in
  p.pokemontype
  ;;

  let get_stored_pokemon_power (iden : string) : int =
  let p = Hashtbl.find !symbols_table iden in
  p.power
  ;;

  (* Children of the tree are both pokemon *)
  let pokemon_fight_ast_aux p1 p2 : ast_node tree =
  match p1, p2 with
  | Node(Leaf l_type, l_n, Leaf l_power), Node(Leaf r_type, r_n, Leaf r_power) ->
    let p = pokemon_fight l_type.data_type (int_of_string l_power.value) r_type.data_type (int_of_string r_power.value) in
      if p = 1 then p1 else p2
  ;;

  let rec pokemon_fight_ast tree : ast_node tree =
    let rec aux t : ast_node tree =
      match t with
      | Node ( Node(l_left, l_n, l_right), n, Node(r_left, r_n, r_right)) ->
        if l_n.token = "Pokemon" && r_n.token = "Pokemon" then
          let p1 = Node(l_left, l_n, l_right) in
          let p2 = Node(r_left, r_n, r_right) in
          pokemon_fight_ast_aux p1 p2;
        else if l_n.token = "Pokemon" && r_n.token = "FIGHT" then
          let fight_node = Node(r_left, r_n, r_right) in
          let p1 = Node(l_left, l_n, l_right) in
          let p2 = aux fight_node in
          pokemon_fight_ast_aux p1 p2;
        else if l_n.token = "FIGHT" && r_n.token = "Pokemon" then
          let fight_node = Node(l_left, l_n, l_right) in
          let p2 = Node(r_left, r_n, r_right) in
          let p1 = aux fight_node in
          pokemon_fight_ast_aux p1 p2;
        else
          let p1 = Node(l_left, l_n, l_right) in p1 (* this should never get here *)
    in aux tree
  ;;

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
  Statements EOL                                { let t = Node_OneChild($1, {data_type="Pokemon_Type"; value="Main"; token="Main"} ) in draw t; 1}
  ;

  Statements:
  | Statement                                   { Node_OneChild($1, {data_type="Pokemon_Type"; value="Statements"; token="Statements"} ) }
  ;

  Statement:
  | Assignment                                  { Node_OneChild($1, {data_type="Pokemon_Type"; value="Statement"; token="Statement"} )}
  | Fight                                       { pokemon_fight_ast $1 ; Node_OneChild($1, {data_type="Pokemon_Type"; value="Statement"; token="Statement"} )} /* do calculation here */
  ;

  Assignment:
  | Declaration ASSIGN Literal                   { Node(
    $1,
    {data_type="Pokemon_Type"; value="ASSIGN"; token="ASSIGN"},
    $3) } /* TODO: type check $3 aka literal to be an int, If valid insert into hashtbl */
  ;

  Declaration:
  | Pokemon_Type IDENTIFIER                     { Node(
    $1,
    {data_type="Pokemon_Type"; value="DECLARATION"; token="DECLARATION"},
    Leaf {data_type="string"; value=$2; token="IDENTIFIER"}
    );}
  ;

  Literal:
  | INT                                         {Leaf {data_type="int"; value=string_of_int $1; token="LITERAL"}}
  | IDENTIFIER                                  {Leaf {data_type="string"; value=$1; token="LITERAL"}}
  ;

  Pokemon_Type:
  | FIRE        {Leaf {data_type="FIRE";value="FIRE";token="Pokemon_Type"}}
  | GRASS       {Leaf {data_type="GRASS";value="GRASS";token="Pokemon_Type"}}
  | ELECTRIC    {Leaf {data_type="ELECTRIC";value="ELECTRIC";token="Pokemon_Type"}}
  | WATER       {Leaf {data_type="WATER";value="WATER";token="Pokemon_Type"}}
  ;

  Pokemon:
  | Pokemon_Type Literal                {if get_type_from_leaf_node($2) = "int"
  then
  Node($1, {data_type="Pokemon"; value="Pokemon"; token="Pokemon"}, $2)
  else
  typecheck_error "Invalid Type"}
  | IDENTIFIER                          {if Hashtbl.mem !symbols_table $1
    then  Node (
      Leaf {data_type="Pokemon_Type"; value=get_stored_pokemon_type $1; token="Pokemon_Type"},
      {data_type="Pokemon"; value=$1; token="IDENTIFIER"},
      Leaf {data_type="int"; value=string_of_int (get_stored_pokemon_power $1); token="LITERAL"})
    else
    name_error $1
  }
  ;

  Fight:
  | Fight FIGHT Pokemon               { Node($1 , {data_type="string"; value="fight"; token="FIGHT"}, $3);}
  | Pokemon                            { $1 }
  ;

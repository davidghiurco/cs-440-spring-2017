/* File parser.mly */
%token <int> INT
%token PLUS
%token POKEMON FIGHT FIRE WATER ELECTRIC GRASS
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
| expr PLUS expr          {print_string ">> "; $1 + $3 }
;

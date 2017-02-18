 /* File parser.mly */
%token <pokemon> POKEMON 
%token FIRE WATER ELETRIC GRASS FIGHT
%token EOL
%left FIGHT        	/* lowest precedence */
%start main             /* the entry point */
%type <int> main
%%
main:
    expr EOL                { $1 }
;
expr:
	INT                      { $1 }
	| FIRE
	| WATER
	| ELECTRIC
	| GRASS
	| expr FIGHT expr          { $1 fight $3 }
;

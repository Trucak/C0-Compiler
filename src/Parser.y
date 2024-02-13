{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

-- operations
'+'       { TOK_PLUS }
'-'       { TOK_MINUS }
'*'       { TOK_MULT }
'/'       { TOK_DIV }
'%'       { TOK_MOD }

-- operators
'='       { TOK_EQ }
"=="      { TOK_EQTO }
"!="      { TOK_DIFF }
'<'       { TOK_LESS }
"<="      { TOK_LESSEQ }
'>'       { TOK_GREATER }
">="      { TOK_GREATEREQ }
"&&"      { TOK_AND }
"||"      { TOK_OR }
'!'       { TOK_NOT }
'^'       { TOK_XOR }

-- separators
'('       { TOK_LPAREN }
')'       { TOK_RPAREN }
'{'       { TOK_LBRACK }
'}'       { TOK_RBRACK }
'['       { TOK_LSQBRACK }
']'       { TOK_RSQBRACK }
';'       { TOK_SEMICOLON }
','       { TOK_COMMA }

-- types
bool             { TOK_TYPE_BOOL }
int              { TOK_TYPE_INT }
float            { TOK_TYPE_FLOAT }
stringType       { TOK_TYPE_STRING }
charType         { TOK_TYPE_CHAR }
void             { TOK_TYPE_VOID }

-- built-in functions
while        { TOK_WHILE }
return       { TOK_RETURN }
if           { TOK_IF }
else         { TOK_ELSE }
for          { TOK_FOR }
continue     { TOK_CONTINUE }
break        { TOK_BREAK }

-- booleans
true        { TOK_TRUE }
false       { TOK_FALSE }

-- numbers
num          { TOK_NUM_INT $$ }
real         { TOK_NUM_REAL $$ }
string       { TOK_STRING $$ }
char         { TOK_CHAR $$ }

-- variables and other functions (identifiers) and misc
ident       { TOK_IDENT $$ }
null        { TOK_NULL }

%nonassoc "==" "!=" '>' ">=" '<' "<=" "&&" "||" '^' '!'
%left '+' '-'
%left '*' '/' '%'

%%

-- Functions

Prog : Func                                           { [$1] }    -- A program might have 1 function (main)
     | Func Prog                                      { $1:$2 }   -- Or multiple

Func : Type ident '(' FuncArgs ')' ';'                { FuncDecl $1 $2 $4 }   -- A function declaration contains return type, function name (identifier) and arguments
     | Type ident '(' ')' ';'                         { FuncDecl $1 $2 [] }   -- It might have no arguments
     | Type ident '(' FuncArgs ')' '{' Stmts '}'      { FuncDefn $1 $2 $4 $7 }   -- A function definition has a return type, name (identifier), arguments and instructions
     | Type ident '(' ')' '{' Stmts '}'               { FuncDefn $1 $2 [] $6 }   -- It might have no arguments

FuncArgs : FuncArg                                    { [$1] }    -- There might be 1 argument
         | FuncArg ',' FuncArgs                       { $1:$3 }   -- Or multiple

FuncArg : Type ident                                  { ArgVar $1 $2 }   -- An argument might be a simple variable
        | Type '[' ']' ident                          { ArgArr $1 $4 }   -- Or an array

-- Statements

Stmts : Stmt                               { [$1] }
      | Stmt Stmts                         { $1:$2 }

Stmt : Simple ';'                          { $1 }
     | Complex                             { $1 }

Simple : return Exp                        { Return $2 }
       | ident '=' Exp                     { Assign $1 $3 }
       | ident '+' '+'                     { Assign $1 (Add (Var $1) (Num 1)) }
       | ident '-' '-'                     { Assign $1 (Sub (Var $1) (Num 1)) }
       | ident '[' Exp ']' '=' Exp         { AssignArr $1 $3 $6 }
       | Exp                               { SpecialExp $1 }   -- Usually reserved for function calls
       | Type Decls                        { Decls $1 $2 }     -- A simple statement might be a declaration
       | continue                          { Continue }
       | break                             { Break }

Complex : while '(' Exp ')' CondStmts                         { While $3 $5 }
        | if '(' Exp ')' CondStmts                            { If $3 $5 }
        | if '(' Exp ')' CondStmts else CondStmts             { IfElse $3 $5 $7 }
        | for '(' Simple ';' Exp ';' Simple ')' CondStmts     { For $3 $5 $7 $9 }

CondStmts : Stmt                           { [$1] }   -- A single statement inside a condition might not need brackets
          | '{' Stmts '}'                  { $2 }     -- But multiple statements do require them

Type : bool                                { TypeBool }
     | int                                 { TypeInt }
     | float                               { TypeFloat }
     | stringType                          { TypeString }
     | charType                            { TypeChar }
     | void                                { TypeVoid }

Decls : Decl                               { [$1] }    -- A single line can be used to declare a single variable
      | Decl ',' Decls                     { $1:$3 }   -- Or multiple variables

Decl : ident                               { Decl $1 }            -- A declaration might be a simple variable
     | ident '[' Exp ']'                   { DeclArr $1 $3 }      -- Or an array
     | ident '=' Exp                       { Init $1 $3 }         -- Update: Init was moved to Decl for simplification
     | ident '[' Exp ']' '=' Exp           { InitArr $1 $3 $6 }   

-- Expressions

Exp : '(' Exp ')'              { $2 }
    | num                      { Num $1 }
    | real                     { Real $1 }
    | string                   { String $1 }
    | char                     { Char $1 }
    | true                     { BoolValue True }
    | false                    { BoolValue False }
    | ident                    { Var $1 }
    | ident '[' Exp ']'        { Arr $1 $3 }
    | Exp '+' Exp              { Add $1 $3 }
    | Exp '-' Exp              { Sub $1 $3 }
    | Exp '*' Exp              { Mul $1 $3 }
    | Exp '/' Exp              { Div $1 $3 }
    | Exp '%' Exp              { Mod $1 $3 }
    | Exp "==" Exp             { Equal $1 $3 }
    | Exp "!=" Exp             { NotEqual $1 $3 }
    | Exp '<' Exp              { LessThan $1 $3 }
    | Exp "<=" Exp             { LessEqThan $1 $3 }
    | Exp '>' Exp              { GreatThan $1 $3 }
    | Exp ">=" Exp             { GreatEqThan $1 $3 }
    | Exp "&&" Exp             { And $1 $3 }
    | Exp "||" Exp             { Or $1 $3 }
    | Exp '^' Exp              { Xor $1 $3 }
    | '!' Exp                  { Not $2 }
    | ident '(' Args ')'       { CallFunc $1 $3 }   -- An expression might consist of a function call
    | ident '(' ')'            { CallFunc $1 [] }   -- That might not have arguments to be given
    | null                     { Null }

Args : Exp                     { [$1] }
     | Exp ',' Args            { $1:$3 }

{
parseError :: [Token] -> a
parseError toks = error "parse error"  
}
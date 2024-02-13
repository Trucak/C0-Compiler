{
module Lexer where
import Numeric (readHex)
}
%wrapper "basic"

$white =  [\ \t\n\r]
$digit =  [0-9]
$hex = [0-9a-fA-F]
$alpha =  [A-Za-z_]  -- _ -> vars e funcs podem conter "espaços" 
$comment = $printable # \n   -- um comentario aceita todos os carateres exceto \n, que sinaliza o fim do comentario

tokens :-

$white+    ;   -- ignorar carateres "brancos"

-- operations
"+"       { \_ -> TOK_PLUS }
"-"       { \_ -> TOK_MINUS }
"*"       { \_ -> TOK_MULT }
"/"       { \_ -> TOK_DIV }
"%"       { \_ -> TOK_MOD }

-- operators
"="       { \_ -> TOK_EQ }
"=="      { \_ -> TOK_EQTO }
"!="      { \_ -> TOK_DIFF }
"<"       { \_ -> TOK_LESS }
"<="      { \_ -> TOK_LESSEQ }
">"       { \_ -> TOK_GREATER }
">="      { \_ -> TOK_GREATEREQ }
"&&"      { \_ -> TOK_AND }
"||"      { \_ -> TOK_OR }
"!"       { \_ -> TOK_NOT }
"^"       { \_ -> TOK_XOR }

-- separators
"("       { \_ -> TOK_LPAREN }
")"       { \_ -> TOK_RPAREN }
"{"       { \_ -> TOK_LBRACK }
"}"       { \_ -> TOK_RBRACK }
"["       { \_ -> TOK_LSQBRACK }
"]"       { \_ -> TOK_RSQBRACK }
";"       { \_ -> TOK_SEMICOLON }
","       { \_ -> TOK_COMMA }

-- types
"bool"     { \_ -> TOK_TYPE_BOOL }
"int"      { \_ -> TOK_TYPE_INT }
"float"    { \_ -> TOK_TYPE_FLOAT }
"string"   { \_ -> TOK_TYPE_STRING }
"char"     { \_ -> TOK_TYPE_CHAR }
"void"     { \_ -> TOK_TYPE_VOID }

-- built-in functions
"while"      { \_ -> TOK_WHILE }
"return"     { \_ -> TOK_RETURN }
"if"         { \_ -> TOK_IF }
"else"       { \_ -> TOK_ELSE }
"for"        { \_ -> TOK_FOR }
"continue"   { \_ -> TOK_CONTINUE }
"break"      { \_ -> TOK_BREAK }

-- booleans
"true"    { \_ -> TOK_TRUE }
"false"   { \_ -> TOK_FALSE }

-- defining types
$digit+                  { \s -> TOK_NUM_INT (read s) }
"0x"$hex+                { \(x:y:xs) -> TOK_NUM_INT (fst (head (readHex xs)))}  -- x = '0', y = 'x'
$digit+"."$digit+        { \s -> TOK_NUM_REAL (read s) }
\"$printable*\"          { \s -> TOK_STRING s }
\'$printable\'           { \s -> TOK_CHAR s }

-- variables and other functions (identifiers)
"NULL"                   { \_ -> TOK_NULL }   -- This is here to prevent being considered a variable
$alpha($alpha|$digit)*   { \s -> TOK_IDENT s}

"//"$comment*            ;

{

data Token
    = TOK_PLUS
    | TOK_MINUS
    | TOK_MULT
    | TOK_DIV
    | TOK_MOD
    | TOK_EQ
    | TOK_EQTO
    | TOK_DIFF
    | TOK_LESS
    | TOK_LESSEQ
    | TOK_GREATER
    | TOK_GREATEREQ
    | TOK_AND
    | TOK_OR
    | TOK_XOR
    | TOK_NOT
    | TOK_LPAREN
    | TOK_RPAREN
    | TOK_LBRACK
    | TOK_RBRACK
    | TOK_LSQBRACK
    | TOK_RSQBRACK
    | TOK_SEMICOLON
    | TOK_COMMA
    | TOK_TYPE_BOOL
    | TOK_TYPE_INT
    | TOK_TYPE_FLOAT
    | TOK_TYPE_STRING
    | TOK_TYPE_CHAR
    | TOK_TYPE_VOID
    | TOK_WHILE
    | TOK_RETURN
    | TOK_IF
    | TOK_ELSE
    | TOK_FOR
    | TOK_CONTINUE
    | TOK_BREAK
    | TOK_TRUE
    | TOK_FALSE
    | TOK_NUM_INT Int
    | TOK_NUM_REAL Float
    | TOK_STRING String
    | TOK_CHAR String
    | TOK_IDENT String
    | TOK_NULL 
    deriving (Eq, Show)

}
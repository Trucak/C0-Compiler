module AST where

-- Functions

type Prog = [Func]

type Ident = String

data Type = TypeBool
          | TypeInt
          | TypeFloat
          | TypeString
          | TypeChar
          | TypeVoid
          | TypeFunc [Type] Type  -- added for typecheck
          deriving (Show, Eq)

data Func = FuncDecl Type Ident [FuncArg]
          | FuncDefn Type Ident [FuncArg] [Stmt]
          deriving Show

data FuncArg = ArgVar Type Ident
             | ArgArr Type Ident
             deriving Show

--- Statements

data Stmt = While Exp [Stmt]
          | Return Exp
          | If Exp [Stmt]
          | IfElse Exp [Stmt] [Stmt]
          | For Stmt Exp Stmt [Stmt]
          | Assign Ident Exp
          | AssignArr Ident Exp Exp
          | SpecialExp Exp
          | Decls Type [Decl]
          | Continue
          | Break
          deriving Show

data Decl = Decl Ident
          | DeclArr Ident Exp 
          | Init Ident Exp   -- Init was moved to Decl for simplification
          | InitArr Ident Exp Exp
          deriving Show

--- Expressions

data Exp = Num Int
         | Real Float
         | String String
         | Char String
         | BoolValue Bool
         | Var Ident
         | Arr Ident Exp
         | RefVar Ident
         | RefArr Ident Exp
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Equal Exp Exp
         | NotEqual Exp Exp
         | LessThan Exp Exp
         | LessEqThan Exp Exp
         | GreatThan Exp Exp
         | GreatEqThan Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Xor Exp Exp
         | Not Exp
         | CallFunc Ident [Exp]
         | Null
         deriving Show
module Typecheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

-- type environment (i.e. symbol table)
type TypeEnv = Map Ident Type

----------------------------------------------------------------------------------
-- Program
----------------------------------------------------------------------------------

-- iterate over all functions of the program
checkProg :: TypeEnv -> Prog -> TypeEnv
checkProg env (x:xs) = checkProg env' xs
                           where env' = checkFunc env x
checkProg env [] = env

----------------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------------

-- iterate over function arguments
checkFunc :: TypeEnv -> Func -> TypeEnv
-- checkFunc env (FuncDefn t ident args) = Map.insert ident (TypeFunc (checkFuncArgTypes args) t) env
checkFunc env (FuncDefn t ident args stmts) 
  = if (checkStmts env'' t stmts) == True then env'
    else error "error in stmts"
        where env'  = Map.insert ident (TypeFunc (checkFuncArgTypes args) t) env
              env'' = checkFuncArgs env' args

-- get data types of args
checkFuncArgTypes :: [FuncArg] -> [Type]
checkFuncArgTypes (x:xs) = (checkFuncArgType x):(checkFuncArgTypes xs)
checkFuncArgTypes [] = []

checkFuncArgType :: FuncArg -> Type
checkFuncArgType (ArgVar t ident) = t
checkFuncArgType (ArgArr t ident) = t

-- check arg types
checkFuncArgs :: TypeEnv -> [FuncArg] -> TypeEnv
checkFuncArgs env ((ArgVar t ident):xs) = checkFuncArgs env' xs
                                           where env' = Map.insert ident t env
checkFuncArgs env ((ArgArr t ident):xs) = checkFuncArgs env' xs
                                           where env' = Map.insert ident t env
checkFuncArgs env [] = env

----------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------

-- check stmt blocks
checkStmts :: TypeEnv -> Type -> [Stmt] -> Bool
checkStmts env t (x:xs) = if (env' == Map.empty) then False
                          else checkStmts env' t xs
                              where env' = checkStmt env t x
checkStmts env _ [] = True


checkStmt :: TypeEnv -> Type -> Stmt -> TypeEnv
-- check variable type stmts
checkStmt env t (Decls t0 decl) = checkDecls env t0 decl
checkStmt env t (Assign ident exp) = let t1 = checkExp env (Var ident)
                                         t2 = checkExp env exp
                                     in if t1==t2 then env
                                        else error "type error: assign type error"
checkStmt env t (AssignArr ident exp1 exp2) = let t1 = checkExp env (Arr ident exp1)
                                                  t2 = checkExp env exp2
                                              in if t1==t2 then env
                                                 else error "type error: assign type error"
-- check conditional stmts
checkStmt env t (While cond stmts)
  = let t0 = checkExp env cond
    in if (t0 == TypeBool) && (checkStmts env t stmts) then env
       else error "type error: condition should be bool"
checkStmt env t (If cond stmts)
  = let t0 = checkExp env cond                           
    in if (t0 == TypeBool) && (checkStmts env t stmts) then env
       else error "type error: if condition should be bool"
checkStmt env t (IfElse cond stmts1 stmts2)
  = let t0 = checkExp env cond                           
    in if (t0 == TypeBool) && 
      (checkStmts env t stmts1) && 
      (checkStmts env t stmts2) then env
       else error "type error: ifelse condition should be bool"
checkStmt env t (For stmt1 cond stmt2 stmts)
  = let env'  = checkStmt env t stmt1
        t0    = checkExp env' cond
        env'' = checkStmt env' t stmt2
    in if (t0 == TypeBool) && (checkStmts env'' t stmts) then env
       else error "type error: for condition is invalid"
-- check flow control operations
checkStmt env t (Return exp)
  = let t0 = checkExp env exp
    in if (t0 == t) then env
       else error "type error: return type error"
checkStmt env t (SpecialExp exp)
  = let t0 = checkExp env exp
    in Map.delete "%FUNCTION_PLACEHOLDER%" env'  -- placeholder since this doesnt check func call
        where env' = Map.insert "%FUNCTION_PLACEHOLDER%" TypeVoid env
checkStmt env t (Continue) = env
checkStmt env t (Break) = env

----------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------

checkExp :: TypeEnv -> Exp -> Type
-- check value types
checkExp _ (Num _) = TypeInt
checkExp _ (Real _) = TypeFloat
checkExp _ (String _) = TypeString
checkExp _ (BoolValue _) = TypeBool
checkExp _ (Char _) = TypeChar
-- check variable types
checkExp env (Var x) = case Map.lookup x env of
    Nothing -> error "undeclared variable"
    Just t -> t
checkExp env (Arr x exp)
  = let t1 = checkExp env exp
        t2 = checkExp env (Var x)
    in if (t1 == TypeInt) then t2
       else error "type error in arr"
-- check arithmetic operations
checkExp env (Add e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeInt
       else error "type error in add"
checkExp env (Sub e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeInt
       else error "type error in sub"
checkExp env (Mul e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeInt
       else error "type error in mul"
checkExp env (Div e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeInt
       else error "type error in div"
checkExp env (Mod e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeInt
       else error "type error in mod"
-- check conditional operations
checkExp env (Equal e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) ||
          (t1 == TypeBool) && (t2 == TypeBool) then TypeBool
       else error "type error in =="
checkExp env (NotEqual e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) ||
          (t1 == TypeBool) && (t2 == TypeBool) then TypeBool
       else error "type error in !="
checkExp env (LessThan e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeBool
       else error "type error in <"
checkExp env (LessEqThan e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeBool
       else error "type error in <="
checkExp env (GreatThan e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeBool
       else error "type error in >"
checkExp env (GreatEqThan e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeInt) && (t2 == TypeInt) then TypeBool
       else error "type error in >="
-- check logical operations
checkExp env (And e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeBool) && (t2 == TypeBool) then TypeBool
       else error "type error in &&"
checkExp env (Or e1 e2) 
  = let t1 = checkExp env e1 
        t2 = checkExp env e2
    in if (t1 == TypeBool) && (t2 == TypeBool) then TypeBool
       else error "type error in ||"
checkExp env (Not e1) 
  = let t1 = checkExp env e1 
    in if (t1 == TypeBool) then TypeBool
       else error "type error in negation"
-- check function calls
checkExp env (CallFunc ident args)
  = if (getArgs env ident args) then case Map.lookup ident env of
                                    Just x  -> case x of
                                        TypeFunc ts t -> t
                                        _  -> error "function call error"
                                    Nothing -> error "function call error"
    else error "function call error"

getArgs :: TypeEnv -> Ident -> [Exp] -> Bool
getArgs env ident exps = case Map.lookup ident env of
                            Just func -> checkArgs env (getTypes func) exps
                            Nothing -> error "function args error"

getTypes :: Type -> [Type]
getTypes (TypeFunc ts t) = ts
getTypes _ = error "function arg types error"

-- check function args
checkArgs :: TypeEnv -> [Type] -> [Exp] -> Bool
checkArgs env (t:ts) (exp:exps) = let t0 = checkExp env exp
                                  in if (t == t0) then checkArgs env ts exps
                                     else False
checkArgs env [] [] = True  -- this function checks 1 arg per call, true if all args-types match
checkArgs env _ [] = error "function call error: args missing types"
checkArgs env [] _ = error "function call error: args missing exps"

-- check variable declarations
checkDecls :: TypeEnv -> Type -> [Decl] -> TypeEnv
checkDecls env t (x:xs) = checkDecls env' t xs
                               where env' = checkDecl env t x
checkDecls env t [] = env

checkDecl :: TypeEnv -> Type -> Decl -> TypeEnv
checkDecl env t (Decl ident) = case Map.lookup ident env of
                                  Just _  -> error "decl error"
                                  Nothing -> Map.insert ident t env
checkDecl env t (DeclArr ident exp) = case Map.lookup ident env of
                                         Just _  -> error "decl error"
                                         Nothing -> let t0 = checkExp env exp
                                                    in if (t0 == TypeInt) then Map.insert ident t env
                                                       else error "type error in decl arr"
checkDecl env t (Init ident exp) = case Map.lookup ident env of
                                      Just _  -> error "init error"
                                      Nothing -> let t0 = checkExp env' exp
                                                 in if (t0 == t) then env'
                                                    else error "type error in decl"
                                                     where env' = Map.insert ident t env
checkDecl env t (InitArr ident exp1 exp2) = case Map.lookup ident env of
                                               Just _  -> error "init error"
                                               Nothing -> let t1 = checkExp env' exp1
                                                              t2 = checkExp env' exp2
                                                          in if (t1 == TypeInt) &&
                                                                (t2 == t) then env'
                                                             else error "type error in decl arr"
                                                              where env' = Map.insert ident t env
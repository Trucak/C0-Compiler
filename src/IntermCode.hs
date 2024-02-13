module IntermCode where

import AST

import Control.Monad.State

import           Data.Map(Map)
import qualified Data.Map as Map

-- Data/Types

type Temp = String
type Label = String

data Inst = MOVE Temp Temp              
          | MOVEI Temp Int              
          | MOVEC Temp Ident [Temp] Int     
          | OP BinOp Temp Temp Temp     
          | OPI BinOp Temp Temp Int     
          | LABEL Label
          | JUMP Label
          | COND Temp RelOp Temp Label Label
          | CALL Ident [Temp] Int
          | RETURN Temp
          deriving Show

data BinOp = OpPlus | OpMinus | OpMult | OpDiv deriving Show
data RelOp = Lt | Gt | Eq | LtEq | GtEq | NEq deriving Show

type Function = (Ident, [Temp], [Inst])
type Table = Map Ident Temp
type Count = (Int, Int)

-- Temps and Labels

newTemp :: State Count Temp
newTemp = do (temps, labels) <- get
             put (temps+1, labels)
             return ("t" ++ show temps)

popTemp :: Int-> State Count ()
popTemp n = do (temps, labels) <- get
               put (temps-n,labels)
               return  ()


resetTemp :: State Count ()
resetTemp = do (temps, labels) <- get
               put (0, labels)
               return  ()

newLabel :: State Count Label
newLabel = do (temps, labels) <- get
              put (temps, labels+1)
              return ("L" ++ show labels)

-- Functions

transProg :: Prog -> Table -> State Count [Function]
transProg (x:prog) table =
     do  (table', fn) <- transFunc x table
         pg <- transProg prog table
         return (fn : pg)
transProg [] table = return []

transFunc :: Func -> Table -> State Count (Table, Function)
transFunc (FuncDefn _ id dc stmts) table =
     do (table', temps) <- transFuncArgs dc table
        (table'', stmts) <- transStmts stmts table'
        return (table'',(id, temps, stmts))

transFuncArgs :: [FuncArg] -> Table -> State Count (Table, [Temp])
transFuncArgs [] table    = return (table, [])
transFuncArgs ((ArgVar typ ident):xs) table =
     do t <- newTemp
        let table' = Map.insert ident t table
        (table'', temps) <- transFuncArgs xs table'
        return (table'', (t:temps))

-- Statements

transStmts :: [Stmt] -> Table -> State Count (Table, [Inst])
transStmts [] table = return (table, [])
transStmts (x:xs) table =
            do  (table', code1) <-  (transStmt x table)
                (table'', code2) <- (transStmts xs table')
                return (table'', (code1 ++ code2))


transStmt :: Stmt -> Table -> State Count (Table, [Inst])
transStmt (Assign ident exp) table = case Map.lookup ident table of
                                    Nothing   -> error "undefined variable"
                                    Just dest -> do code1 <- transExp exp table dest
                                                    return (table, code1)
transStmt (If exp stmts) table =
     do  l1    <- newLabel
         l2    <- newLabel
         code1 <- transCond exp table l1 l2
         (_, code2) <- transStmts stmts table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]))

transStmt (IfElse exp stmts1 stmts2) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         code1 <- transCond exp table l1 l2
         (_, code2) <- transStmts stmts1 table
         (_', code3) <- transStmts stmts2 table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3]))

transStmt (While exp stmts) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         code1 <- transCond exp table l2 l3
         (_, code2) <- transStmts stmts table
         return (table, ([LABEL l1] ++ code1 ++ [LABEL l2] ++ code2 ++ [JUMP l1, LABEL l3]))

transStmt (For stmt1 e1 stmt2 stmts) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         (table', code1) <- transStmt stmt1 table
         code2 <- transCond e1 table' l2 l3
         (_, code3) <- transStmt stmt2 table'
         (_, code4) <- transStmts stmts table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ code3 ++ [LABEL l2] ++ code4 ++ [JUMP l1, LABEL l3]))

transStmt (AssignArr id exp1 exp2) table =
     do  t0  <- newTemp
         t1  <- newTemp
         t2  <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(3)
         return (table, ([MOVE t0 t1] ++ code1 ++ [OPI OpMult t0 t0 4] ++ code2 ++ [MOVE t0 t2]))
        
transStmt (Decls _ decls) table =
     do  (table', code1) <- transDecls  decls table
         return (table', code1)

transStmt (Return e1) table =
     do t0 <- newTemp
        code1 <- transExp e1 table t0
        popTemp(1)
        return (table, (code1 ++ [RETURN t0]))

transStmt (SpecialExp (CallFunc id exps)) table =
     do  (code, temps) <- transExps exps table
         let ntemps = Map.size table
         return (table, (code ++ [CALL id temps ntemps]))

-- Declarations

transDecls :: [Decl] -> Table -> State Count (Table, [Inst])
transDecls [] table = return (table, [])
transDecls (x:xs) table = do (table', code1) <- transDecl x table
                             (table'', code2) <- transDecls xs table'
                             return (table'', code1 ++ code2)


transDecl :: Decl -> Table -> State Count (Table, [Inst])
transDecl (Decl ident) table = do t <- newTemp
                                  let table' = Map.insert ident t table
                                  return (table',[MOVEI t 0])

{-do  (code1, addr) <- transIndex (Arr id exp) table
    return (code1 ++ [MOVE dest addr])-}

transDecl (DeclArr ident exp) table = do t1 <- newTemp
                                         t2 <- newTemp
                                         let table' = Map.insert ident t1 table
                                         code1 <- transExp exp table' t2
                                         return (table',[MOVE t1 t2] ++ code1 ++ [MOVEI t1 0])
transDecl (Init ident exp) table = do t <- newTemp
                                      let table' = Map.insert ident t table
                                      code1 <- transExp exp table' t
                                      return (table',code1)
-- Expessions

transExps :: [Exp] -> Table -> State Count ([Inst],[Temp])
transExps [] _ = return ([],[])
transExps (x:xs) table =
     do  t1           <- newTemp
         ex           <- transExp x table t1
         (recI, recT) <- transExps xs table
         popTemp(1)
         return (ex ++ recI, t1 : recT)


transExp :: Exp -> Table -> Temp -> State Count [Inst]
transExp (BoolValue False) _ dest = return [MOVEI dest 0]
transExp (BoolValue True) _ dest = return [MOVEI dest 1]
transExp (Num n) _ dest = return [MOVEI dest n]
transExp (Var x) table dest = case Map.lookup x table of
                            Nothing -> error "undefinied variable"
                            Just n -> do
                                        return [MOVE dest n]

transExp (Add e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExp e1 table t1
        code2 <- transExp e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpPlus dest t1 t2])

transExp (Sub e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExp e1 table t1
        code2 <- transExp e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpMinus dest t1 t2])

transExp (Mul e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExp e1 table t1
        code2 <- transExp e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpMult dest t1 t2])

transExp (Div e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExp e1 table t1
        code2 <- transExp e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpDiv dest t1 t2])

transExp (Mod e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        t3    <- newTemp
        t4    <- newTemp
        code1 <- transExp e1 table t1
        code2 <- transExp e2 table t2
        popTemp(4)
        return (code1 ++ code2 ++ [OP OpDiv t3 t1 t2, OP OpMult t4 t3 t2, OP OpMinus dest t1 t4])

transExp (LessThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (LessThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])

transExp (LessEqThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (LessEqThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExp (GreatThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (GreatThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])

transExp (GreatEqThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (GreatEqThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])

transExp (Equal e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (Equal e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])

transExp (NotEqual e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (NotEqual e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])

transExp (Arr id exp) table dest =
    do  (code1, addr) <- transIndex (Arr id exp) table
        return (code1 ++ [MOVE dest addr])

transExp (CallFunc id exps) table dest =
    do  (code, temps) <- transExps exps table
        let ntemps = Map.size table
        return (code ++ [MOVEC dest id temps ntemps])

-- Array index

transIndex :: Exp -> Table -> State Count ([Inst], Temp)
transIndex (Arr id e1) table =
    do base  <- case Map.lookup id table of
               Nothing -> error "undefined variable"
               Just n -> return n
       addr  <- newTemp
       code1 <- transExp e1 table addr
       return (code1 ++ [OPI OpMult addr addr 4, OP OpPlus addr addr base], addr)

-- Conditions

transCond :: Exp -> Table -> Label -> Label -> State Count [Inst]
transCond (Not exp) table lt lf = transCond exp table lf lt
transCond (LessEqThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 LtEq t2 lt lf])

transCond (LessThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Lt t2 lt lf])

transCond (GreatThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Gt t2 lt lf])

transCond (GreatEqThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 GtEq t2 lt lf])

transCond (Equal exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Eq t2 lt lf])

transCond (NotEqual exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExp exp1 table t1
         code2 <- transExp exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 NEq t2 lt lf])

transCond (Or exp1 exp2) table lt lf =
     do  l1    <- newLabel
         code1 <- transCond exp1 table l1 lf
         code2 <- transCond exp2 table lt lf
         return (code1 ++ [LABEL l1] ++ code2)

transCond (And exp1 exp2) table lt lf =
     do  l1    <- newLabel
         code1 <- transCond exp1 table lt l1
         code2 <- transCond exp2 table lt lf
         return (code1 ++ [LABEL l1] ++ code2)

transCond (CallFunc ident exp) table lt lf =
     do  t1    <- newTemp
         code1 <- transExp (CallFunc ident exp) table t1
         return (code1)

transCond (BoolValue True) _ lt _ = return []
transCond (BoolValue False) _ _ lf = return []
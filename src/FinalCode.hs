module FinalCode where

import IntermCode

finalTransProg :: [Function] -> String
finalTransProg funcs = initProg ++ finalTransProgAux funcs ++ finalProg

finalTransProgAux :: [Function] -> String
finalTransProgAux (("main", tmps, insts):xs) = functionTranslate "main" (length tmps) ++ finalTransInsts insts ++ finalTransProgAux xs ++ "li $v0, 10\nsyscall\n\n"
finalTransProgAux (("read_int", tmps, insts):xs) = "" ++ finalTransProgAux xs
finalTransProgAux (("print_int", tmps, insts):xs) = "" ++ finalTransProgAux xs
finalTransProgAux (("scan_int", tmps, insts):xs) = "" ++ finalTransProgAux xs
finalTransProgAux ((ident, tmps, insts):xs) =  finalTransProgAux xs ++ functionTranslate ident (length tmps) ++ finalTransInsts insts
finalTransProgAux [] = ""

initProg :: String
initProg = "\n.globl main\n\n.text\n\n"

finalProg :: String
finalProg = "print_int:\nli $v0, 1\nlw $a0, 0($sp)\nsyscall\njr $ra\n\nread_int:\nli $v0, 5\nsyscall\njr $ra\n\nscan_int:\nli $v0, 5\nsyscall\nmove $v0, $a0\njr $ra"

finalTransInsts :: [Inst] -> String
finalTransInsts (i:insts) = (pogTranslate i) ++ "\n" ++ (finalTransInsts insts)
finalTransInsts [] = []

functionTranslate :: Label -> Int -> String
functionTranslate label n = "\n" ++ label ++ ":\n" ++ "sw $fp, -4($sp)\n" ++ "sw $ra, -8($sp)\n" ++ "la $fp, 0($sp)\n" ++ "la $sp, -"++ (show (4*n)) ++"($sp)\n" ++ argsHandler n

argsHandler :: Int -> String
argsHandler 0 = ""
argsHandler n = "lw $t"++ (show n) ++", "++ (show ((n-1)*4)) ++"($fp)\n" ++ argsHandler (n-1)

pogTranslate :: Inst -> String
pogTranslate (MOVE  t1 t2)    = "addu " ++ "$" ++ t1 ++ ", $zero, " ++ "$" ++ t2
pogTranslate (MOVEI t1 im)    = "addi " ++ "$" ++ t1 ++ ", $zero, " ++ (show im)
pogTranslate (LABEL label)    = label ++ ":"
pogTranslate (JUMP label)     = "j " ++ label
pogTranslate (OP binop t1 t2 dest)   = (opTranslate binop) ++ "$" ++ t1 ++ ", " ++ "$" ++ t2 ++ ", "++ "$" ++ dest    --who's who?
pogTranslate (OPI binop t1 dest im)  = (opiTranslate binop) ++ ", " ++ "$" ++ t1 ++ ", "++ "$" ++ dest ++ ", " ++ (show im)
pogTranslate (COND t1 relop t2 label1 label2) = (relopTranslate relop) ++ "$" ++ t1 ++ ", " ++ "$" ++ t2 ++ ", " ++ label1 ++ "\n" ++ pogTranslate (JUMP label2)
pogTranslate (RETURN t1) = "move $v0, " ++ "$" ++ t1 ++ "\n" ++ "la $sp, 0($fp)\n" ++ "lw $ra, -8($sp)\n" ++ "lw $fp, -4($sp)\n" ++ "jr $ra\n"
pogTranslate (CALL ident temps ntemps) = saveTemps ntemps ++ saveRegs 0 temps ++ "jal " ++ ident ++ "\n" ++ restoreRegs temps ++ "\n"++ restoreTemps ntemps
pogTranslate (MOVEC t1 ident temps ntemps) = pogTranslate (CALL ident temps ntemps) ++ "\n" ++ "addu " ++ "$" ++ t1 ++ ", $zero, $v0"

saveTemps :: Int -> String
saveTemps 1 = "addu $s0, $zero, $t0"++"\n"
saveTemps i = "addu $s"++(show (i-1))++", $zero, $t"++(show (i-1))++"\n" ++ saveTemps (i-1)

restoreTemps :: Int -> String
restoreTemps 1 = "addu $t0, $zero, $s0"
restoreTemps i = "addu $t"++(show (i-1))++", $zero, $s"++(show (i-1))++"\n" ++ restoreTemps (i-1)

saveRegs :: Int -> [Temp] -> String
saveRegs i [] = if i==0 then ""
                else "la $sp, " ++ (show i) ++ "($sp)" ++ "\n"
saveRegs i temps = saveReg (i-4) (last temps) ++ saveRegs (i-4) (init temps)

saveReg :: Int -> Temp -> String
saveReg i temp = "sw " ++"$" ++ temp ++ ", " ++ (show i) ++ "($sp)" ++ "\n"

restoreRegs :: [Temp] -> String
restoreRegs temps = "la $sp, " ++ (show ((length temps)*4)) ++ "($sp)"

opTranslate :: BinOp -> String
opTranslate OpPlus  = "add "
opTranslate OpMinus = "sub "
opTranslate OpMult  = "mul "
opTranslate OpDiv   = "div "

opiTranslate :: BinOp -> String
opiTranslate OpPlus  = "addi "
opiTranslate OpMinus = "subi "
opiTranslate OpMult  = "muli "
opiTranslate OpDiv   = "divi "

relopTranslate :: RelOp -> String
relopTranslate Lt   = "blt "
relopTranslate LtEq = "ble "
relopTranslate Gt   = "bgt "
relopTranslate GtEq = "bge "
relopTranslate Eq   = "beq "
relopTranslate NEq  = "bne "
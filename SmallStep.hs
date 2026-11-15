
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | SevenTimes C   ---- Executa o comando C 7 vezes
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Assert B C     --- caso B avalie para True então executa C. Caso contrário, não faz nada
   deriving(Eq,Show)

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a variável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v _ = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)
smallStepE (Num n, s)                  = (Num n, s)


smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not TRUE, s)                = (FALSE, s)
smallStepB (Not FALSE, s)               = (TRUE, s)
smallStepB (Not b, s)                   = let (bl,sl) = smallStepB (b,s)
                                          in (Not bl, sl)
smallStepB (And TRUE b2, s)             = (b2, s)
smallStepB (And FALSE _, s)             = (FALSE, s)
smallStepB (And b1 b2, s)               = let (bl,sl) = smallStepB (b1,s)
                                          in (And bl b2, sl)
smallStepB (Or TRUE _, s)               = (TRUE, s)
smallStepB (Or FALSE b2, s)             = (b2, s)
smallStepB (Or b1 b2, s)                = let (bl,sl) = smallStepB (b1,s)
                                          in (Or bl b2, sl)
smallStepB (Leq (Num n1) (Num n2), s)   = if n1 <= n2 then (TRUE, s) else (FALSE, s)
smallStepB (Leq (Num n) e, s)           = let (el,sl) = smallStepE (e,s)
                                          in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)               = let (el,sl) = smallStepE (e1,s)
                                          in (Leq el e2, sl)
smallStepB (Igual (Num n1) (Num n2), s) = if n1 == n2 then (TRUE, s) else (FALSE, s)
smallStepB (Igual (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                          in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)             = let (el,sl) = smallStepE (e1,s)
                                          in (Igual el e2, sl)
smallStepB (TRUE, s)                    = (TRUE, s)
smallStepB (FALSE, s)                   = (FALSE, s)


smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If TRUE c1 _, s)       = (c1, s)
smallStepC (If FALSE _ c2, s)      = (c2, s)
smallStepC (If b c1 c2, s)         = let (bl,sl) = smallStepB (b,s)
                                     in (If bl c1 c2, sl)

smallStepC (Seq Skip c2, s)        = (c2, s)
smallStepC (Seq c1 c2, s)          = let (cl,sl) = smallStepC (c1,s)
                                     in (Seq cl c2, sl)

smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s)       = let (el,sl) = smallStepE (e,s)
                                        in (Atrib (Var x) el, sl)

smallStepC (While b c, s)          = (If b (Seq c (While b c)) Skip, s)

-- SevenTimes c: desdobra em 7 execuções seguidas de c
smallStepC (SevenTimes c, s)       = (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c c))))), s)

-- DoWhile c b: executa c uma vez e depois se comporta como While b c
smallStepC (DoWhile c b, s)        = (Seq c (While b c), s)

-- DuplaATrib (Var v1) (Var v2) e1 e2: v1 := e1 ; v2 := e2
smallStepC (DuplaATrib (Var v1) (Var v2) e1 e2, s) = (Seq (Atrib (Var v1) e1) (Atrib (Var v2) e2), s)

-- AtribCond b (Var v1) e1 e2: avalia b e escolhe qual atribuição fazer
smallStepC (AtribCond TRUE (Var v1) e1 _, s)  = (Atrib (Var v1) e1, s)
smallStepC (AtribCond FALSE (Var v1) _ e2, s) = (Atrib (Var v1) e2, s)
smallStepC (AtribCond b (Var v1) e1 e2, s)    = let (bl,sl) = smallStepB (b,s)
                                                in (AtribCond bl (Var v1) e1 e2, sl)

-- Swap (Var x) (Var y): troca o conteúdo das duas variáveis em um passo
smallStepC (Swap (Var x) (Var y), s) = let vx = procuraVar s x
                                           vy = procuraVar s y
                                       in (Skip, mudaVar (mudaVar s x vy) y vx)

-- Unless b c1 c2: se b for falso executa c1, senão executa c2
smallStepC (Unless FALSE c1 _, s)  = (c1, s)
smallStepC (Unless TRUE _ c2, s)   = (c2, s)
smallStepC (Unless b c1 c2, s)     = let (bl,sl) = smallStepB (b,s)
                                     in (Unless bl c1 c2, sl)

-- Assert b c: se b for verdadeiro executa c, senão não faz nada
smallStepC (Assert TRUE c, s)      = (c, s)
smallStepC (Assert FALSE _, s)     = (Skip, s)
smallStepC (Assert b c, s)         = let (bl,sl) = smallStepB (b,s)
                                     in (Assert bl c, sl)

smallStepC (Skip, s)               = (Skip, s)
smallStepC (c, _)                  = error ("Comando invalido: " ++ show c)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num _) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False


interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False


interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

-- *Main> interpretadorB (teste1, exSigma2)
-- (TRUE,[("x",3),("y",0),("z",0)])

-- *Main> interpretadorB (teste2, exSigma2)
-- (TRUE,[("x",3),("y",0),("z",0)])

-- *Main> interpretadorB (teste2, exSigma)
-- (FALSE,[("x",10),("temp",0),("y",0)])


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z")))

-- *Main> interpretadorC (testec1, exSigma2)
-- (Skip,[("x",0),("y",3),("z",3)])

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- *Main> interpretadorC (fatorial, exSigma2)
-- (Skip,[("x",1),("y",6),("z",0)])


---
-- Exemplos com os comandos novos:

exSigma3 :: Memoria
exSigma3 = [("x",5), ("y",2), ("z",0)]

-- SevenTimes: soma 2 em z sete vezes (z = 14)
testeSevenTimes :: C
testeSevenTimes = SevenTimes (Atrib (Var "z") (Soma (Var "z") (Num 2)))

-- *Main> interpretadorC (testeSevenTimes, exSigma3)
-- (Skip,[("x",5),("y",2),("z",14)])

-- DoWhile: decrementa x até chegar em 0, contando as voltas em z
testeDoWhile :: C
testeDoWhile = DoWhile (Seq (Atrib (Var "x") (Sub (Var "x") (Num 1)))
                            (Atrib (Var "z") (Soma (Var "z") (Num 1))))
                       (Not (Leq (Var "x") (Num 0)))

-- *Main> interpretadorC (testeDoWhile, exSigma3)
-- (Skip,[("x",0),("y",2),("z",5)])

-- DoWhile com condição falsa desde o início: o corpo executa uma vez
testeDoWhile2 :: C
testeDoWhile2 = DoWhile (Atrib (Var "z") (Num 99)) FALSE

-- *Main> interpretadorC (testeDoWhile2, exSigma3)
-- (Skip,[("x",5),("y",2),("z",99)])

-- DuplaATrib: x := 1 e y := x + 10
testeDuplaAtrib :: C
testeDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Num 1) (Soma (Var "x") (Num 10))

-- *Main> interpretadorC (testeDuplaAtrib, exSigma3)
-- (Skip,[("x",1),("y",11),("z",0)])

-- AtribCond: se x <= y então z := 100 senão z := 200
testeAtribCond :: C
testeAtribCond = AtribCond (Leq (Var "x") (Var "y")) (Var "z") (Num 100) (Num 200)

-- *Main> interpretadorC (testeAtribCond, exSigma3)
-- (Skip,[("x",5),("y",2),("z",200)])

-- Swap: troca x e y
testeSwap :: C
testeSwap = Swap (Var "x") (Var "y")

-- *Main> interpretadorC (testeSwap, exSigma3)
-- (Skip,[("x",2),("y",5),("z",0)])

-- Unless: como x == 5 é verdadeiro, executa o segundo comando (z := 2)
testeUnless :: C
testeUnless = Unless (Igual (Var "x") (Num 5)) (Atrib (Var "z") (Num 1)) (Atrib (Var "z") (Num 2))

-- *Main> interpretadorC (testeUnless, exSigma3)
-- (Skip,[("x",5),("y",2),("z",2)])

-- Assert: só faz z := x * y se y <= x
testeAssert :: C
testeAssert = Assert (Leq (Var "y") (Var "x")) (Atrib (Var "z") (Mult (Var "x") (Var "y")))

-- *Main> interpretadorC (testeAssert, exSigma3)
-- (Skip,[("x",5),("y",2),("z",10)])

-- Assert com condição falsa: não altera a memória
testeAssert2 :: C
testeAssert2 = Assert (And TRUE (Igual (Var "x") (Num 0))) (Atrib (Var "z") (Num 7))

-- *Main> interpretadorC (testeAssert2, exSigma3)
-- (Skip,[("x",5),("y",2),("z",0)])

-- Programa combinando vários comandos: calcula 2^x em y usando DoWhile,
-- depois troca x e y com Swap
potencia :: C
potencia = Seq (Atrib (Var "y") (Num 1))
               (Seq (DoWhile (Seq (Atrib (Var "y") (Mult (Var "y") (Num 2)))
                                  (Atrib (Var "x") (Sub (Var "x") (Num 1))))
                             (Not (Igual (Var "x") (Num 0))))
                    (Swap (Var "x") (Var "y")))

-- *Main> interpretadorC (potencia, exSigma3)
-- (Skip,[("x",32),("y",0),("z",0)])

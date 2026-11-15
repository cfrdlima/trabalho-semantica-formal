
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
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




ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,_) = n
ebigStep (Soma e1 e2,s) = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s) = ebigStep (e1,s) - ebigStep (e2,s) -- (subtração)
ebigStep (Mult e1 e2,s) = ebigStep (e1,s) * ebigStep (e2,s) -- (multiplicação)
ebigStep (Div e1 e2,s)
   | divisor == 0 = error "Divisao por zero"
   | otherwise    = ebigStep (e1,s) `div` divisor
   where divisor = ebigStep (e2,s)


bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,_) = True
bbigStep (FALSE,_) = False

bbigStep (Not b,s) =
   not (bbigStep (b,s))

bbigStep (And b1 b2,s) =
   bbigStep (b1,s) && bbigStep (b2,s)

bbigStep (Or b1 b2,s) =
   bbigStep (b1,s) || bbigStep (b2,s)

bbigStep (Leq e1 e2,s) =
   ebigStep (e1,s) <= ebigStep (e2,s)

bbigStep (Igual e1 e2,s) =
   ebigStep (e1,s) == ebigStep (e2,s) -- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais

cbigStep :: (C,Memoria) -> (C,Memoria)

-- Skip: nao altera a memoria
cbigStep (Skip,s) = (Skip,s)

-- If: executa um dos dois comandos conforme a condicao
cbigStep (If b c1 c2,s)
   | bbigStep (b,s) = cbigStep (c1,s)
   | otherwise = cbigStep (c2,s)

-- Seq: executa dois comandos em sequencia
cbigStep (Seq c1 c2,s) =
   let (_,s1) = cbigStep (c1,s)
       (_,s2) = cbigStep (c2,s1)
   in (Skip,s2)
   
-- Atrib: atribui o resultado de uma expressao a uma variavel
cbigStep (Atrib (Var x) e,s) =
   (Skip,mudaVar s x (ebigStep (e,s)))

-- While: executa o comando enquanto a condicao for verdadeira
cbigStep (While b c,s)
   | bbigStep (b,s) =
       let (_,s1) = cbigStep (c,s)
       in cbigStep (While b c,s1)
   | otherwise = (Skip,s)

-- SevenTimes: executa o comando exatamente sete vezes
cbigStep (SevenTimes c,s) =
   let repetir :: Int -> Memoria -> Memoria
       repetir 0 estado = estado
       repetir n estado =
          let (_,novoEstado) = cbigStep (c,estado)
          in repetir (n-1) novoEstado
   in (Skip,repetir 7 s)

-- DoWhile: executa primeiro e testa a condicao depois
cbigStep (DoWhile c b,s) =
   let (_,s1) = cbigStep (c,s)
   in executar s1
   where
      executar estado
         | bbigStep (b,estado) =
             let (_,s1) = cbigStep (c,estado)
             in executar s1
         | otherwise = (Skip,estado)

-- DuplaATrib: realiza duas atribuicoes simultaneas
cbigStep (DuplaATrib (Var v1) (Var v2) e1 e2,s) =
   let valor1 = ebigStep (e1,s)
       valor2 = ebigStep (e2,s)
       s1 = mudaVar s v1 valor1
       s2 = mudaVar s1 v2 valor2
   in (Skip,s2)

-- AtribCond: escolhe a expressao conforme a condicao
cbigStep (AtribCond b (Var v) e1 e2,s)
   | bbigStep (b,s) =
       (Skip,mudaVar s v (ebigStep (e1,s)))
   | otherwise =
       (Skip,mudaVar s v (ebigStep (e2,s)))

-- Swap: troca os valores de duas variaveis
cbigStep (Swap (Var v1) (Var v2),s) =
   let valor1 = procuraVar s v1
       valor2 = procuraVar s v2
       s1 = mudaVar s v1 valor2
       s2 = mudaVar s1 v2 valor1
   in (Skip,s2)

-- Unless: executa c1 se a condicao for falsa
cbigStep (Unless b c1 c2,s)
   | not (bbigStep (b,s)) = cbigStep (c1,s)
   | otherwise = cbigStep (c2,s)

-- Assert: executa o comando somente se a condicao for verdadeira
cbigStep (Assert b c,s)
   | bbigStep (b,s) = cbigStep (c,s)
   | otherwise = (Skip,s)

-- Qualquer outro caso (ex.: atribuicao para algo que nao e variavel) e invalido
cbigStep (c,_) = error ("Comando invalido: " ++ show c)
   
--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM TODOS OS COMANDOS NOVOS:
-- * SevenTimes
-- * DoWhile
-- * DuplaAtribuicao
-- * AtribCond
-- * Swap
-- * Unless
-- * Assert
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

progNovos1 :: C
progNovos1 =
   foldr1 Seq
   [
      SevenTimes
         (Atrib (Var "x")
                (Soma (Var "x") (Num 1))),

      DoWhile
         (Atrib (Var "y")
                (Soma (Var "y") (Num 1)))
         (Leq (Var "y") (Num 2)),

      DuplaATrib (Var "x") (Var "y")
                 (Var "y") (Var "x"),

      AtribCond (Leq (Var "x") (Var "y"))
                (Var "z")
                (Var "x")
                (Var "y"),

      Swap (Var "x") (Var "y"),

      Unless (Igual (Var "x") (Var "y"))
             (Atrib (Var "z") (Num 99))
             (Atrib (Var "z") (Num 20)),

      Assert (Leq (Var "z") (Num 100))
             (Atrib (Var "z")
                    (Soma (Var "z") (Num 1)))
   ]

exSigma3 :: Memoria
exSigma3 = [("x",2),("y",5),("z",1)]

progNovos2 :: C
progNovos2 =
   foldr1 Seq
   [
      SevenTimes
         (Atrib (Var "y")
                (Sub (Var "y") (Num 1))),

      DoWhile
         (Atrib (Var "z")
                (Soma (Var "z") (Num 1)))
         (Leq (Var "z") (Num 3)),

      DuplaATrib (Var "x") (Var "y")
                 (Soma (Var "x") (Num 10))
                 (Mult (Var "z") (Num 2)),

      AtribCond (Leq (Var "x") (Var "y"))
                (Var "z")
                (Num 50)
                (Num 30),

      Swap (Var "x") (Var "z"),

      Unless (Leq (Var "x") (Var "y"))
             (Atrib (Var "y") (Num 100))
             (Atrib (Var "y") (Num 200)),

      Assert (Leq (Var "z") (Var "x"))
             (Atrib (Var "y")
                    (Soma (Var "y") (Num 1)))
   ]

---
--- Exemplos com divisao e casos de borda dos comandos novos:

progDiv :: E
progDiv = Div (Soma (Var "x") (Num 4)) (Num 2)

-- DoWhile com condicao falsa: o corpo executa uma vez mesmo assim
testeDoWhile :: C
testeDoWhile = DoWhile (Atrib (Var "z") (Num 9)) FALSE

-- Assert com condicao falsa: nao altera a memoria
testeAssert :: C
testeAssert = Assert (Igual (Var "x") (Num 0)) (Atrib (Var "z") (Num 9))


---
--- Resultados esperados:
---
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progDiv, exSigma2)
-- 3
-- *Main> bbigStep (teste1, exSigma2)
-- True
-- *Main> bbigStep (teste2, exSigma2)
-- True
-- *Main> bbigStep (teste2, exSigma)
-- False
-- *Main> cbigStep (testec1, exSigma2)
-- (Skip,[("x",0),("y",3),("z",3)])
-- *Main> cbigStep (fatorial, exSigma2)
-- (Skip,[("x",1),("y",6),("z",0)])
-- *Main> cbigStep (progNovos1, exSigma2)
-- (Skip,[("x",10),("y",3),("z",100)])
-- *Main> cbigStep (progNovos1, exSigma3)
-- (Skip,[("x",9),("y",6),("z",100)])
-- *Main> cbigStep (progNovos2, exSigma3)
-- (Skip,[("x",30),("y",101),("z",12)])
-- *Main> cbigStep (testeDoWhile, exSigma2)
-- (Skip,[("x",3),("y",0),("z",9)])
-- *Main> cbigStep (testeAssert, exSigma2)
-- (Skip,[("x",3),("y",0),("z",0)])

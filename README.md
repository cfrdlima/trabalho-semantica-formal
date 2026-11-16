# Semântica Formal — Interpretador Big-Step

Trabalho da disciplina de Semântica Formal.

Implementação, em Haskell, da semântica operacional big-step de uma linguagem
imperativa simples, com expressões aritméticas, expressões booleanas e comandos.

## Autores

- Claudinei de Lima
- Lyon Falcão

## A linguagem

**Expressões aritméticas (`E`):** `Num`, `Var`, `Soma`, `Sub`, `Mult`, `Div`

**Expressões booleanas (`B`):** `TRUE`, `FALSE`, `Not`, `And`, `Or`, `Leq` (≤), `Igual`

**Comandos (`C`):**

| Comando | Significado |
|---|---|
| `Skip` | não faz nada |
| `Atrib x e` | `x := e` |
| `Seq c1 c2` | executa `c1` e depois `c2` |
| `If b c1 c2` | se `b`, executa `c1`; senão, `c2` |
| `While b c` | repete `c` enquanto `b` for verdadeiro |
| `SevenTimes c` | executa `c` sete vezes |
| `DoWhile c b` | executa `c` ao menos uma vez e repete enquanto `b` |
| `DuplaATrib x y e1 e2` | `x := e1` e `y := e2` de forma simultânea |
| `AtribCond b x e1 e2` | se `b`, `x := e1`; senão, `x := e2` |
| `Swap x y` | troca o conteúdo de `x` e `y` |
| `Unless b c1 c2` | se `b` for falso, executa `c1`; senão, `c2` |
| `Assert b c` | executa `c` somente se `b` for verdadeiro |

## Funções principais

- `ebigStep :: (E, Memoria) -> Int` — avalia expressões aritméticas
- `bbigStep :: (B, Memoria) -> Bool` — avalia expressões booleanas
- `cbigStep :: (C, Memoria) -> (C, Memoria)` — executa comandos e devolve a memória final

A memória é uma lista de pares `(nome, valor)`, por exemplo `[("x",3), ("y",0), ("z",0)]`.

## Como executar

Requisito: [GHC](https://www.haskell.org/ghc/).

ghci Semantica.hs

Exemplos:

ghci> ebigStep (progExp1, exSigma)
13
ghci> cbigStep (fatorial, exSigma2)
(Skip,[("x",1),("y",6),("z",0)])
ghci> cbigStep (progNovos1, exSigma2)
(Skip,[("x",10),("y",3),("z",100)])

Todos os programas de teste e os resultados esperados estão comentados no final de `Semantica.hs`.

## Erros tratados

- Variável não declarada na memória
- Divisão por zero
- Comando inválido (por exemplo, atribuição a algo que não é variável)

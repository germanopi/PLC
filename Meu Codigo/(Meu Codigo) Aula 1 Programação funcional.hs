:t () -- Diz o tipo do arqumento
:t funcao -- Diz o tipo dos argumentos que a funcao recebe e que retorna  
:load x.hs -- Carrega o arquivo
:q -- Sai do ghci
:r -- Recarrega arquivo
/= -- diferente
Executar -- Abre Visual Code, baixa haskelly, cria arquivo hsig, renomeia para .hs, baixa haskell, reinicia o pc, new terminal, run file, ghci no terminal, :load x.hs no terminal
Definir funcao -- No inicio da linha 
Paradigma -- estilo de programação imperativo (variavel,atribuição), OO, Funcional (primeira ordem, não colateral) e concorrente
Programação funcional -- paradigma onde programas são definicoes de dados e funcoes , executar é avaliar expressoes, funcoes sao de 1 ordem (podem ser argumentos) e sem efeito colateral
Interpretador -- infere tipos
-- Comentarios
{- Comentarios -}

funcaoConstante :: Integer -- Define que a funcao retorna um Integer
funcaoConstante = 30 -- Não é atribuição, o corpo da funcao que é 30, funcao começa letra minuscula
funcaoConstante -- No terminal retorna o corpo da funcao que é 30

menorQue30 :: Integer -> Bool -- Define que a funcao recebe um Integer e retorna um Booleano
menorQue30 x = x < 30   -- x é o argumento para ser utilizado no corpo da funcao que ao ser avaliado retorna um Booleano

"aaa"++"bbb" -- Concatena as strings para aaabbb

quadrado :: Integer -> Integer -- Define que a funcao recebe integer e retorna integer
quadrado x = x * x  -- x é o argumento para o corpo da funcao
quadrado 3 -- No terminal retorna o integer 9 como resultado

valoresIguais :: Integer -> Integer -> Integer -> Bool -- Define que a funcao recebe 3 argumentos do tipo Integer e retorna um Booleano
valoresIguais x y z = ( x == y && y == z ) -- x, y, z são argumentos para o corpo da funcao, que retorna o Bool do calculo a direita da igualdade 
valoresIguais 1 2 3 -- No terminal, dá false
valoresIguais 1 1 1 -- No terminal, dá true


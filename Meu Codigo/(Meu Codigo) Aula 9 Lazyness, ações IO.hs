-- Lazyness 

-- só avalia expressões quando o valor é necessario

somaInt :: Int -> Int -> Int
somaInt a b = a + b

{-
somaInt (9-3) (f 3 5) = (9-3) + (f 3 5)
somaInt (9-3) (f 3 5) = 6 + 8
somaInt (9-3) (f 3 5) = 14

-}

-- avalia somente os argumentos necessarios para execução 

somaPrimeiroLista :: [Int] -> [Int] -> Int
somaPrimeiroLista (a:as) (b:bs) = a + b

somaPrimeiroLista [1..] [2..] -- resulta em [3..]

-- não faz avaliação duplicada 

{-
somaInt (9-3) (9-3) = (9-3) + (9-3)
somaInt (9-3) (9-3) = 6    +    6  -- avalia (9-3) apenas 1 vez e troca em cada ocorrencia
somaInt (9-3) (9-3) = 12
-}

-- Chamada de Cauda 

fat n = tailFat n 1 -- resultado é retornado por quem fez a chamada (fat)
tailFat 0 x = x
tailFat n x = tailFat(n-1)(n*x)

main :: IO () -- retorna o resultado mas quem calcula é o showStackHead
main = do
     let  s1 = []
     r1 <- showStackHead s1
     putStrLn $ " retornou" ++ r1 -- $ faz o ghci avaliar primeira o lado direito para depois printar

showStackHead [] = return [] -- retorna a lista vazia
showStackHead (x : xs )= do  -- retorna a cabeça da lista 
        putStrLn $ "resultado" ++ [x] -- $ faz o ghci avaliar primeiro o lado direito para depois printar
        return xs
    
-- Input e Output 

imprimeHello :: IO ()  -- função que realiza ação I/O
imprimeHello = putStrLn "Hello World" -- imprime 'Hello World'

imprime :: String -> IO () -- função que recebe uma string e realiza uma ação I/O
imprime str = putStrLn str -- imprime a String recebida 

getLine :: IO String -- lê uma linha do teclado
getChar :: IO Char -- lê um Char do teclado 

putStr :: String -> IO () -- imprime uma linha na tela 
putStrLn :: String -> IO () -- imprime uma linha na tela e dá quebra de linha 

imprime4vezes :: String -> IO ()
imprime4vezes str = do putStr str -- 'do' permite sequencias de ações I/O
                       putStr str
                       putStr str
                       putStr str

imprimeNvezes :: Int -> String -> IO ()
imprimeNvezes n str
    = if n <= 1     -- operação if-then 
        then putStr str
        else do putStr str
            imprimeNvezes (n-1) str

atribuiEntrada :: IO ()  -- função que realiza ação I/O
atribuiEntrada = do line <- getLine -- line recebe a linha de entrada

imprimeEntrada = do l <- getLine
                 putStrLn l
                 q <- getLine
                 putStrLn l
                 putStrLn q

{-
Exemplo
main :: IO()  
main = do putStr "Digite seu nome:" -- imprime 'Digite seu nome:'
        st <- getLine               -- st recebe a linha de entrada
        putStr “Ao contrario e':"   -- imprime 'Ao contrario e :'
        putStr (reverse st)         -- imprime o reverso de st
-}





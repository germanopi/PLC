
reverterListaInt :: [Int] -> [Int] -- reverte a ordem dos elementos da lista 
reverterListaInt [] = [] 
reverterListaInt (x:xs) = reverterListaInt xs ++ [x]

repeticao :: Int -> Char -> [Char] -- repete n vezes o char de entrada
repeticao 0 ch = []
repeticao n ch = ch : repeticao (n-1) ch

take n [] -- retorna os n primeiros elementos da lista

take 2 [1..7] -- retorna [1,2] 
{-
funcao do mtake
take _ [] = []
take 0 _ = []
take n (x:xs) = x: take (n-1) xs
-}

drop n [] -- retorna todos os elementos da lista , exceto os n primeiros 

drop 2 [1..7] -- retorna [3,4,5,6,7]
{-
funcao do drop
drop 0 lista = lista
drop _ [] = [] 
drop n (x:xs) = drop (n-1) xs

-}

init [] -- retorna todos os elementos da lista exceto o ultimo

init [1..7]  -- retorna [1,2,3,4,5,6]

last [] -- retorna o ultimo elemento da lista 

last [1..7] -- retorna 7

head [] -- retorna o primeiro elemento da lista 

head [1..7] -- retorna 1

length [] -- retorna o tamanho da lista 

length [1..7] -- retorna 7
  
-- Ordenação

iSort :: [Int] -> [Int] -- ordena uma lista 
iSort [] = []
iSort (a:as) = ins a (iSort as)

ins :: Int -> [Int] -> [Int] -- adiciona e ordena elemento na lista 
ins x [] = [x]
ins x (y:ys)
 | x <= y = x : (y:ys)
 | otherwise = y : ins x ys

 -- Expressão case (permite casamento de padrão com valor arbitrario, não apenas argumentos de funções)

head1 :: [a] -> a  -- retorna a cabeça da lista ou ativa uma exceção 
head1 [] = error "lista vazia"
head1 (x:_) = x

head2 :: [a]-> a -- retorna a cabeça da lista ou ativa uma exceção 
head2 xs = case xs  of  
   [] -> error "lista vazia"
   (x:_) -> x

formaDaLista :: [a] -> String 
formaDaLista xs = "Uma lista " ++ case xs of  -- retorna o formato da lista no caso da lista ser vazia, um elemento ou varios
                         [] -> "vazia "
                         [x] -> "com um elemento" 
                         (x:xs) -> "com mais de um elemento" 


-- Polimorfismo ( função com tipo generico)


zipar :: [t] -> [u] -> [(t,u)] -- agrupa os elementos das listas na ordem 
zipar (a:as) (b:bs) = (a,b) : zipar as bs -- zipi [1,2,3] [4,5,6] retorna [(1,4),(2,5),(3,6)]
zipar _ _ = []

length :: [t] -> Int
length [ ] = 0
length (a:as) = 1 + length as

-- Compreensão de Listas ( Definir listas em função de outra lista )

dobrarLista :: [Int] -> [Int] 
dobrarLista l = [ 2*x | x <- l]

ehPar :: Int -> Bool -- retorna True ou false se o mod de x mod 2 for igual a zero
ehPar x = (mod x 2) == 0

dobrarListaPar :: [Int] -> [Int]   
dobrarListaPar l = [ 2 * x | x <- l, ehPar x] -- retorna a lista 2*x tal que x está em l e é par 

somarPares :: [(Int,Int)] -> [Int]
somarPares l = [ x + y | (x,y) <- l ] -- retorna a lista x + y tal que (x,y) está em l


[x | x <- [1..7] ] -- retorna [1,2,3,4,5,6,7] ou seja a lista x tal que x que está em [1..7]
[2 * x | x  <- [1..10]] -- retorna [2,4,6,8,10,12,14,16,18,20] ou seja a lista 2*x tal que x está em [1..10]
[(x,x) | x <- [1..5]] -- retorna [(1,1),(2,2),(3,3),(4,4),(5,5)] ou seja a lista (x,x) tal que x está em [1..5]
[2 * x | x  <- [1..10] , x > 5] -- retorna [12,14,16,18,20] ou seja a lista 2*x tal que x está em [1..10] e é maior que 5
[2 * x | x  <- [1..10] , x > 5 && x < 9 && ehpar x ] -- retorna [12,16] seja a lista 2*x tal que x está em [1..10] e é maior que 5 e menor que 9 e é par


-------- QUESTÃO DE PROVA ----------
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort[y| y <- xs, y <= x] ++ 
               [x] ++ 
               qSort[ y | y <- xs, y > x]
  














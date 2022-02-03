-- Uso de Guardas

maiorValor :: Int -> Int -> Int -- Define que a funcao recebe 2 argumentos do tipo Int e retorna um Int
maiorValor x y -- x, y são argumentos para serem usados no corpo da função
 | x >= y = x    -- Guarda 1 se (x>=y) retorna x  
 | x < y = y     -- Guarda 2 se (x<y) retorna y
 | otherwise = y -- Guarda n retorna y

-- Recursão

vendas :: Int -> Int 
vendas 0 = 10
vendas 1 = 20
vendas 2 = 30

maxVendas :: Int -> Int 
maxVendas n
 | n == 0 = vendas 0 -- Caso Base
 | otherwise = max ( vendas n ) ( maxVendas ( n - 1 ) ) -- Caso Recursivo

-- Casamento de Padrão

maxVendas2 :: Int -> Int
maxVendas2 0 = vendas 0
maxVendas2 n = max ( vendas n ) ( maxVendas2 ( n - 1 ) ) 

-- Tuplas (qualquer tipo)

tupla :: (Int,Bool,Char,[Char],(Int,Int)) 
tupla = (2,True,'a',"casa",(1,2))

-- Listas [mesmo tipo]

ordenar :: Int -> [Int] -> [Int] 
ordenar x [] = [x]
ordenar x (y:ys)
 | x <= y    = (x:y:ys)         -- insere x na frente da cabeça se for menor  
 | otherwise = y : ordenar x ys -- se for maior procura o prox elemento 

take n [] -- retorna os n primeiros elementos da lista
drop n [] -- retorna todos os elementos da lista , exceto os n primeiros 
init [] -- retorna todos os elementos da lista exceto o ultimo

-- Compreensão de Listas ( Definir listas em função de outra lista )

dobrarLista :: [Int] -> [Int] 
dobrarLista l = [ 2*x | x <- l] -- dobra o elemento x da lista L, se x está em L

-- Funcoes de alta ordem (recebem ou retornam funções)

somaInt :: Int -> Int-> Int ->Int
somaInt x y z = x + y + z 

aplicaDuasVezes :: (Int -> Int) -> Int  -> Int
aplicaDuasVezes f x = f (f x)  

{-
aplicaDuasVezes (somaInt 3 3) 2 = somaInt 3 3 (somaInt 3 3 2)
aplicaDuasVezes (somaInt 3 3) 2 = somaInt 3 3 8
aplicaDuasVezes (somaInt 3 3) 2 = 14
-}

dobro :: Int ->Int
dobro x = 2*x

map dobro [2,3] = [4,6] -- Map (Aplica uma função a uma lista)
filter (>2) [2,3] = [3] -- filter (retorna elementos da lista aceitos na condição)
foldr1 (*) [2,3] = 6--foldr1 (aplica a operação na lista)

--  Função anonima

(\x -> x + 1) 2 = 3  -- aguarda x e aplica no corpo

-- Classes

type Nome = String
type Potencia = Int
data Lampada = Compacta Nome Potencia | Incandescente Nome Potencia

instance Eq Lampada where
   (Compacta n1 p1) == (Compacta n2 p2)           = n1 == n2 && p1 == p2
   (Incandescente n1 p1) == (Incandescente n2 p2) = n1 == n2 && p1 == p2

instance Show Lampada where
    show (Compacta n1 p1) = "Compacta " ++ n1 ++ " " ++ show p1
 

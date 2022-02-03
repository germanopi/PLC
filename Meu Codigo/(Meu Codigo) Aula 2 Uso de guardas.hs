-- Funcao definida com uso de guardas

maxi :: Integer -> Integer -> Bool
maxi x y
 | x >= y = x -- Guarda1 = Expressao1 (se x>=y retorne x)
 | x < y = y  -- Guarda2 = Expressao2 (se x < y retorne y)
 | otherwise = y -- Guarda n = Expressao n (caso contrario retorne y)

maiorValorIfElse :: Int -> Int -> Int -- é possivel usar if then else inves de guardas para paradigma imperativo ( if expressao ,then comando, else comando) para 
maiorValorIfElse x y = if x >= y then x else y -- programação funcional (if expressao, then expressao, else expressao)

addD a b =2 * (a+b) == 2 * (b+a) = addD b a -- Prova de propriedades programacao funcional verifica corretude de escrita se suas propriedades sao satisfeitas 

--Recursão 

vendas :: Integer -> Integer 
vendas 0 = 5
vendas 1 = 6
vendas 2 = 7
vendas 3 = 8
vendas 4 = 2

totalVendas :: Integer -> Integer 
totalVendas n
 | n == 0 = vendas 0 -- Funcao que usa funcao
 | otherwise = vendas n + totalVendas ( n - 1 )

{- Execucao de totalVendas 2
totalVendas 2 = vendas 2 + totalVendas (2-1)
totalVendas 2 = vendas 2 + vendas 1 + totalVendas (1-1)
totalVendas 2 = vendas 2 + vendas 1 + vendas 0
totalVendas 2 = 5 + 6 + 7
totalVendas 2 = 18
-}

maxiVendas :: Integer -> Integer 
maxiVendas n
 | n == 0 = vendas 0 -- Caso Base
 | otherwise = maxi ( vendas n ) ( maxiVendas ( n - 1 ) ) -- Caso Recursivo

 {- Execucao de maxiVendas 2
maxiVendas 2 = maxi ( vendas 2 ) ( maxiVendas 1 )
maxiVendas 2 = maxi ( vendas 2 ) ( maxi ( vendas 1 ) ( maxVendas 0 ) ) 
maxiVendas 2 = maxi ( vendas 2 ) ( maxi ( vendas 1 ) ( vendas 0 ) ) 
maxiVendas 2 = maxi ( 7 ) ( maxi 6 5 )
maxiVendas 2 = maxi 7 6
maxiVendas 2 = 7
-}

-- Casamento de padrao  (permite usar padroes no lugar das variaveis,na definição de funções)

maxiVendasCasamentoPadrao :: Integer -> Integer  
maxiVendasCasamentoPadrao 0 = vendas 0
maxiVendasCasamentoPadrao n = maxi ( vendas n ) ( maxiVendas (n-1) )

totalVendasCasamentoPadrao :: Integer -> Integer 
totalVendasCasamentoPadrao 0 = vendas 0
totalVendasCasamentoPadrao n = vendas n + totalVendas (n-1)

-- Operações logicas 

myNot :: Bool -> Bool
myNot True = False
myNot False = True
myOr :: Bool -> Bool -> Bool
myOr True True = True
myOr True False = True
myOr False True = True
myOr False False = False
myAnd :: Bool -> Bool -> Bool
myAnd True x = x  -- O segundo elemento é vai decidir o resultado
myAnd False _ = False -- O segundo elemento não é relevante 
myOr2 :: Bool -> Bool -> Bool
myOr2 True x = True 
myOr2 False x = x
myOr3 :: Bool -> Bool -> Bool
myOr3 True _  = True 
myOr3 False x = x



-- EXERCICIOS EM AULA

fatorial :: Int -> Int
fatorial n
 | n == 0  = 1                      -- caso base
 | otherwise =  n * fatorial (n-1)    -- caso recursivo

fatorialCasamPadrao :: Int -> Int
fatorialCasamPadrao 0 = 1
fatorialCasamPadrao n = n * fatorialCasamPadrao (n-1)

all4Equal :: Integer -> Integer -> Integer -> Integer -> Bool
all4Equal x y z w
 | (x==y && y==z && z==w) = True
 | otherwise = False

equalCount :: Integer -> Integer -> Integer -> Integer 
equalCount x y z 
 | ( x == y && y == z && x == z ) = 3
 | ( x /= y && y /= z && x /= z ) = 0
 | otherwise = 2
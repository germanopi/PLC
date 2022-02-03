-- Funcoes de alta ordem (recebem funcoes como argumentos ou retornam funções como resultado)

-- (funcao como argumento) 
-- [lista como argumento]


somaInt :: Int -> Int-> Int ->Int
somaInt x y z = x + y + z 

soma3Int :: Int -> (Int -> (Int -> Int))
soma3Int x y z = x + y + z

aplicaDuasVezes :: (t -> t) -> t  -> t
aplicaDuasVezes f x = f (f x)  

{-

Recebe uma função como argumento e um argumento do tipo t e retorna um argumento do tipo t

aplica parcialmente a função t 

aplicaDuasVezes (somaInt 3 3) 2 = somaInt 3 3 (somaInt 3 3 2)
aplicaDuasVezes (somaInt 3 3) 2 = somaInt 3 3 8
aplicaDuasVezes (somaInt 3 3) 2 = 14

-}

vendas :: Int -> Int
vendas 0 = 4
vendas 1 = 0
vendas 2 = 8
vendas 3 = 5

quadrado :: Int->Int
quadrado x = x * x

total :: (Int -> Int) -> Int -> Int -- é do tipo (função) n -> n, por exemplo "total vendas n"
total f 0 = f 0
total f n = f n + total f (n-1)

totalVendas n = total vendas n -- a funcao totalVendas chama a função total passando a função "vendas" e o tipo "n" como argumentos

{-
totalVendas 3 = total vendas 3 
totalVendas 3 = vendas 3 + total vendas (2)
totalVendas 3 = vendas 3 + vendas 2 + total vendas (1)
totalVendas 3 = vendas 3 + vendas 2 +  vendas 1 + total vendas 0
totalVendas 3 = vendas 3 + vendas 2 +  vendas 1 +  vendas 0
totalVendas 3 = 5 + 8 + 0 + 4
totalVendas 3 = 17   
-}

somaQuadrados n = total quadrado n -- a função somaQuadrados chama a funcao total passando a função "quadrado" e o tipo "n" como argumentos 

{-
somaQuadrados 3 = total quadrado 3 
somaQuadrados 3 = quadrado 3 + total quadrado (2)
somaQuadrados 3 = quadrado 3 + quadrado 2 + total quadrado (1)
somaQuadrados 3 = quadrado 3 + quadrado 2 + quadrado 1 + total quadrado (0)
somaQuadrados 3 = quadrado 3 + quadrado 2 + quadrado 1 + quadrado 0
somaQuadrados 3 = 9 + 4 + 1 + 0
somaQuadrados 3 = 14
-}

maxi :: Int -> Int -> Int
maxi x y
 | x >= y    = x
 | otherwise = y

maxFun :: (Int -> Int) -> Int -> Int -- recebe (uma funcao que recebe um int e retorna um int) depois recebe um int e por fim retorna um int
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f (n-1)) (f n) -- maxFun (maxi 3) 6 retorna 6

-- função map substitui argumentos por outra função 
--Recebe como argumentos (a transformação/função a ser aplicada a cada elemento da lista) e a lista de entrada

dobrarValoresLista :: [Int] -> [Int]
dobrarValoresLista [] = []
dobrarValoresLista (x:xs) = (2*x) : dobrarValoresLista xs

quadradoValoresLista :: [Int]->[Int]
quadradoValoresLista [] = []
quadradoValoresLista (x:xs) = x*x : quadradoValoresLista xs

mapeamento :: (Int->Int)->[Int] ->[Int] -- recebe (uma transformação/funcao que recebe int e retorna int), recebe uma lista de int e retorna uma lista de int
mapeamento f [] = []
mapemaneto f (x:xs) = f x : mapeamento f xs -- aplica a transformação/função a cada elemento da lista, começando pela cabeça

ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

map1 :: (a->b) -> [a] -> [b] -- map1 ehPar [1..7] retorna [FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs  -- aplica a transformação/função a cada elemento da lista ,começando pela cabeça

{-
map1 ehPar [1,2,3] = ehPar [1] : map1 ehPar [2,3]
map1 ehPar [1,2,3] = ehPar [1] : ehPar [2] : map1 ehPar [3]
map1 ehPar [1,2,3] = ehPar [1] : ehPar [2] : ehPar [3] : map1 ehPar []
map1 ehPar [1,2,3] = ehPar [1] : ehPar [2] : ehPar [3] : ehPar []
map1 ehPar [1,2,3] = [False] : [True] : [False] : []
map1 ehPar [1,2,3] = [False,True,False] 
-}

mapCompreensaoLista :: (Int -> Int) -> [Int] -> [Int]
mapCompreensaoLista f l = [f x | x <- l]  -- aplica a função f em x para todo x pertencente a lista l

-- filtro/filter retorna apenas os elementos que são aceitos pela condição , serve para definir listas em funções de outras listas 

filtro :: (a-> Bool) -> [a] -> [a]
filtro f [] = [] 
filtro f (x:xs) 
 | f x = x : filtro f xs     -- x entra na lista de saida se é aceito pela condição f
 | otherwise = filtro f xs -- descarta x se não é aceito na condição f e passa pro proximo elemento da lista de entrada
-- filtro (>5) [1..10]  retorna [6,7,8,9,10]

digitos :: String->String
digitos d = filtro isDigit d


filtroCompreensaoLista :: (Int -> Int) -> [Int] -> [Int]
filtroCompreensaoLista f l = [x | x <- l , f x] -- x está na lista de saida se x está na lista de entrada l e  é aceito pela condição f 

-- Folding Recebe como argumentos (a operação/função a ser aplicada a cada elemento da lista) e a lista de entrada

fold :: (t->t->t)->[t]->t -- fold (||) [True,False,False] retorna True , fold (*) [1..6] retorna 720
fold f [a]  = a
fold f (a:as)= f a (fold f as) 

somaLista :: [Int] -> Int
somaLista []=0
somaLista (x:xs) = x + somaLista xs

somaLista2 :: [Int] -> Int
somaLista2 []=0
somaLista2 (x:xs) = (+) x (somaLista2 xs)

produtoLista :: [Int] -> Int
produtoLista []=1
produtoLista (x:xs) x * (produtoLista xs)

produtoLista2 :: [Int] -> Int
produtoLista2 []=1
produtoLista2 (x:xs) (*) x (produtoLista2 xs)

orLista []=False
orLista (x:xs)=(||) x (orLista xs)

andLista []=True
andLista (x:xs)=(&&) x (andLista xs)

mfoldr :: (t ->t ->t) -> t -> [t]-> t -- recebe a operação a ser aplicada, um Bool e uma lista com Booleanos e retorna um Booleano
mfoldr f v [] = v  
mfoldr f v (x:xs) = f x (mfoldr f v xs) -- No exemplo f é (||) , v é False , x inicial é a cabeça da lista que é True

{-
moldr (||) False [True,False] = (||) True (mfoldr (||) False [False])
moldr (||) False [True,False] = (||) True (||) False (mfoldr (||) False [])
moldr (||) False [True,False] = (||) True ((||) False False)
moldr (||) False [True,False] = (||) True False
moldr (||) False [True,False] = True
-}

mfoldl :: (t ->t ->t) -> t -> [t]-> t -- recebe a operação a ser aplicada, um Bool e uma lista com Booleanos e retorna um Booleano
mfoldl f v [] = v 
mfoldl f v (x:xs) = mfoldl f (f v x) xs -- No exemplo f é (||) , v é False , x inicial é a cabeça da lista que é True

{-
moldl (||) False [True,False] = moldl (||) ((||) False True) [False]
moldl (||) False [True,False] = moldl (||) True [False]
moldl (||) False [True,False] = moldl (||) ((||) True False) []
moldl (||) False [True,False] = moldl (||) True []
moldl (||) False [True,False] = True
-}


--Sinonimo de tipos
type CPF = Int  
type Idade = Int
type Name = String -- Nomeia Name como um tipo do tipo String
type Pessoa = (CPF,Idade,Name) -- O tipo pessoa recebe 

nome :: Pessoa->Name -- name recebe (Int,Int,String) e retorna String
nome (cpf,idade,nome) = n

-- Listas (Deve conter elementos de mesmo tipo, ordem e duplicação de elementos importam)

[] -- lista vazia de qualquer tipo

-- escrita de listas

[1,2,3] == 1:2:3:[]  -- : é um construtor de listas 

:t [1,1,1] :: Num a => [a]  -- Lista com 3 elementos do tipo Int

:t [(1,"a"),(1,"a")] :: Num a => [(a, [Char])] -- Lista com 2 elementos do tipo (Int,[Char]) Onde [Char] é uma lista de caracters ou seja String

:t [(1,"a"),(1,"b"),("c",1)] -- Error de lista pois dois primeiros elementos possuem tipo (Int,[Char]) e o terceiro ([Char],Int)

[-1.5 .. 10] -- gera [-1.5,0.5,1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5]

['a' .. 'd'] --gera "abcd"

[1, 4 .. 50] -- gera [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49] pela diferença dos elementos 

[10 , 8 .. -5]  -- gera [10,8,6,4,2,0,-2,-4]

[2.8,3.3..5.0] = -- gera [2.8,3.3,3.8,4.3,4.8]

[2..2] -- gera [2]

[2,7..4] -- gera [2]

[10..1] -- gera [] 

[10,9..1] -- gera [10,9,8,7,6,5,4,3,2,1]

[2,9,8..1] -- error deve ser [elemento1.. elemento2] ou [elemento1, elemento2 .. elemento3]


-- Funções sobre listas

somaLista :: [Int]->Int
somaLista [] = 0 -- caso base
somaLista (x:xs) = x + somaLista xs -- caso recursivo onde x é a cabeça e xs o resto

{-  
somaLista [1,2,3]
somaLista [1,2,3] = somaLista [1 : [2],[3],[]]
somaLista [1,2,3] = 1 + somaLista [2 : [3],[]]
somaLista [1,2,3] = 1 + 2 + somaLista [3 : []]
somaLista [1,2,3] = 1 + 2 + 3 + somaLista [[]]
somaLista [1,2,3] = 1 + 2 + 3 + 0
somaLista [1,2,3] = 6
-}

-- Funcao de concatenacao de listas : (++) :: [a] -> [a] -> [a]

reverterLista :: [Int] -> [Int] 
reverterLista []     = []
reverterLista (x:xs) = reverterLista xs  ++ [x]

{-
reverterLista [1,2,3]
reverterLista [1,2,3] = reverterLista [2,3] ++ [1]
reverterLista [1,2,3] = reverterLista [3] ++ [2] ++ [1]
reverterLista [1,2,3] = reverterLista [] ++ [3] ++ [2] ++ [1]
reverterLista [1,2,3] = [] ++ [3] ++ [2] ++ [1]
-}

repeticao :: Int -> Char -> String
repeticao 0 ch  = []
repeticao n ch = [ch] ++ repeticao (n-1) ch

{-
repeticao 5 'a' = ['a'] ++ repeticao (4) 'a'
repeticao 5 'a' = ['a'] ++ ['a'] ++ repeticao (3) 'a'
repeticao 5 'a' = ['a'] ++ ['a'] ++ ['a'] ++ repeticao (2) 'a'
repeticao 5 'a' = ['a'] ++ ['a'] ++ ['a'] ++ ['a'] ++ repeticao (1) 'a'
repeticao 5 'a' = ['a'] ++ ['a'] ++ ['a'] ++ ['a'] ++ ['a'] ++ repeticao (0) 'a'
repeticao 5 'a' = ['a'] ++ ['a'] ++ ['a'] ++ ['a'] ++ ['a'] ++ []
repeticao 5 'a' = "aaaaa"
-}

-- ordenacao de listas

ins :: Int -> [Int] -> [Int] 
ins x [] = [x]
ins x (y:ys)
 | x <= y    = (x:y:ys) -- insere x na frente da cabeça se for menor  
 | otherwise = y : ins x ys -- se for maior procura o prox elemento 

iSort :: [Int] -> [Int]
iSort []  = []
iSort (x:xs) = ins x (iSort xs)

-- Exercícios

double :: [Int]->[Int] -- dobrar os elementos de uma lista 
double [] = []
double (x:xs) = 2*x : double xs

member :: [Int] -> Int -> Bool -- determinar se um valor faz parte da lista 
member [] n = False
member (x:xs) n 
 | (x == n) = True
 | otherwise = member xs n 

isDigits :: Char -> Bool -- retorna se é digito
isDigits ch = (ch >= '0') && (ch<='9')

digits :: String -> String -- String é lista de characters , determinar digitos na string 
digits [] = [] 
digits (x:xs)
 | isDigits x = x : digits xs
 | otherwise = digits xs

somaParesLista :: [(Int,Int)] -> [(Int)] -- soma os elementos de cada tupla da lista 
somaParesLista [] = []
somaParesLista (x:xs) = (fst x + snd x) : somaParesLista xs



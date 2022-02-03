-- definições locais
-- Duas formas : where e let ... in 

quadrado :: Int -> Int
quadrado n = n * n

{-
quadrado (2+3) = (2+3) * (2+3)
quadrado (2+3) = 5 * 5
-}

somaQuadrados :: Int -> Int -> Int 
somaQuadrados x y = quadrado x + quadrado y


somaQuadrada :: Integer −> Integer −> Integer
somaQuadrada x y = quadradoX + quadradoY
 where quadradoX = x * x                        -- <definicoes> where <definicoes>
       quadradoY = y * y

somaQuadrada2 :: Int -> Int -> Int 
somaQuadrada2 x y = quadrado x + quadrado y
    where 
        quadrado n = n * n

somaQuadrada3 :: Int -> Int -> Int 
somaQuadrada3 x y = let quadradoX = x * x  -- let <definicoes> in <expressoes> 
                      quadradoY = y * y  -- let diz como vai se comportar os argumentos 
                  in quadradoX + quadradoY -- in é o local que o comportamento vai ser executado

let x = 2* 4 in x + 6 -- Executa a mutiplicação 2 * 4 em x e utiliza o seu resultado na soma (x + 6)

-- Exercicio
 
vendasdaSemana :: Int -> Int
vendasdaSemana 1 = 10
vendasdaSemana 2 = 20
vendasdaSemana 3 = 30 
vendasdaSemana 4 = 40
vendasdaSemana 5 = 50
vendasdaSemana 6 = 60 
vendasdaSemana 7 = 70
vendasdaSemana 8 = 40
vendasdaSemana 9 = 40 

vendasNulas :: Int −> Bool
vendasNulas 0 = ( vendas 0 == 0)
vendasNulas n = ( vendas n == 0 ) -- retorna se houve ou não vendas na semana n

vendasIguais :: Int -> Int -> Int
vendasIguais semanas valor 
 | semanas == 1 && valor == vendasdaSemana 1 = 1 -- ultima semana com valor igual
 | semanas == 1 && valor /= vendasdaSemana 1 = 0 -- ultima semana com valor diferente
 | valor == vendasdaSemana(semanas) && (semanas /= 1) = 1 + vendasIguais (semanas-1) (valor) -- semana qualquer com valor igual
 | otherwise = vendasIguais (semanas-1) (valor) -- semana qualquer com valor diferente
 

-- Tipos basicos em Haskell 

... , -1, 0 , 1 , ... -- são do tipo :: Int
+,*,-,^,div,mod,abs -- são do tipo :: Int ->Int -> Int
>,<,>=,<=,/= -- são do tipo :: Int ->Int -> Bool
True,False -- são do tipo :: Bool
&&,|| -- são do tipo Bool ->Bool->Bool
not -- é do tipo ::Bool->Bool
'a','b' -- são do tipo :: Char
ord  -- é do tipo :: Char ->Int
chr -- é do tipo :: Int -> Char
"abc","casa" -- são do tipo :: String
++ -- é do tipo ::String ->String ->String, Não da pra concatenar characteres

"peixe" ++ "\n" ++ "gato" ++ "\n" -- resulta peixe\ngato\n
putStr ("gato" ++ "\n" ++ "casa" ++ "\n") -- resulta gato na primeira linha e casa na segunda

import Data.Char -- Importa a biblioteca Data.char
:m + Data.Char -- importa o modulo direto do ghci

Int --possui precisao fixa 
Integer -- possui precisao arbitraria

div 3 2 -- retorna 1
3 `div` 2 -- retorna 1
5/2 -- retorna 2.5
mod 5 2  -- retorna 1
5 `mod` 2 -- retorna 1
2^3 -- retorna 8

xor :: Bool -> Bool -> Bool --xor
xor x y = (x || y) && not (x && y)

minBound  -- retorna default [] 
minBound :: Int -- retorna o menor valor dos inteiros , precisão fixa ou seja memoria limitada 
maxBound :: Integer -- dá error porque não é limitado , precisão arbitraria ou seja a quantidade de memoria necessaria para armazenar o inteiro 

ord 'A' -- retorna inteiro correspondente 65
chr 65 -- retorna character correspondente 'A' 


fromEnum :: Char −> Int
toEnum :: Int −> Char
fromEnum 'A' -- retorna o valor para int correspondente a 'A'
toEnum 65  -- retorna o char correspondente ao valor int 65

offset :: Int 
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char  -- retorna a letra maiuscula da entrada
toUpper ch = toEnum ( fromEnum ch + offset ) -- se estiver usando a biblioteca data.char executa com Main.toUpper

isDigit :: Char -> Bool -- retorna se é digito 
isDigit ch = (ch >+ '0'  && (ch <= '9') -- se estiver usando a biblioteca data.char executa com Main.isDigit 

-- Float e Double ( ponto flutuante )

+,−,*,/ -- são do tipo :: Float −> Float −> Float  

-- show :: a -> String 
-- read :: String -> a

(read "2" :: Int) + 5  -- le uma string e transforma para o tipo passado
show 3 -- retorna a string do argumento passado

ceiling , floor, round -- são do tipo :: Float −> Int
pi -- é do tipo :: Float
fromIntegral -- é do tipo :: Int −> Float

-- EXERCICIOS DO SLIDE

addEspacos :: Int -> string
addEspacos 0 = " "
addEspacos n = " " ++ addEspacos(n-1)

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

imprimeTabela :: Int −> String
imprimeTabela n = cabecalho
            ++ imprimeSemanas n
            ++ imprimeTotal n
            ++ imprimeMedia n

-- Estruturas de dados Tuplas 

tupla :: (Int,Int) -- recebe um argumento com 2 elementos
tupla = (2,3)

tupla2 :: (Int,Bool,Char,[Char],(Int,Int)) -- recebe um argumento com 5 elementos de qualquer tipo, inclusive tuplas,  [Char] pois String são listas de char
tupla2 = (2,True,'a',"casa",(1,2))

primeiro :: (a, b) -> a 
primeiro (x,y) = x

segundo :: (a, b) -> b
segundo (x,y) = y 

somaElementos :: (Int, Int) -> Int -- recebe uma tupla contendo 2 argumentos inteiros e retorna um inteiro 
somaElementos p = primeiro p + segundo p  -- primeiro (1,5) retorna 1 e segundo (1,5) retorna 5
somaElementos p = fst p + snd p -- funções do haskell, fst pega o primeiro elemento e snd pega o segundo                          

primeiroInt :: (Int,Int) -> Int
primeiroInt (x,y) = x 

segundoInt :: (Int,Int) -> Int
segundoInt (x,y) = y 

shift :: ((Int,Int),Int) -> (Int,(Int,Int)) -- move o parenteses a direita uma casa
shift ((x,y),z) = (x,(y,z))

-- EXEMPLO EQUAÇÃO SEGUNDO GRAU

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = −b/( 2.0 * a )

twoRoots :: Float -> Float -> Float -> ( Float , Float )
twoRoots a b c = (d−e,d+e)
            where
                d = −b/( 2.0 * a )
                e = sqrt ( b^2 − 4.0 * a * c ) / ( 2.0 * a )

roots :: Float -> Float -> Float -> String
roots a b c
    | b^2 == 4 . 0* a* c = show ( oneRoot a b c )
    | b^2 > 4 . 0* a* c = show f ++ " " ++ show s
    | otherwise = "no roots "
        where ( f , s ) = twoRoots a b c

-- Exercicios

menorMaior :: Int -> Int -> Int -> (Int,Int) -- retorna a tupla com o menor e maior dos 3 argumentos 
menorMaior primeiro segundo terceiro 
 | (primeiro < segundo && primeiro < terceiro) && segundo < terceiro = (primeiro,terceiro) 
 | (primeiro < segundo && primeiro < terceiro) && segundo > terceiro = (primeiro,segundo) 
 | (segundo < primeiro && segundo < terceiro) && primeiro < terceiro = (segundo,terceiro)
 | (segundo < primeiro && segundo < terceiro) && primeiro > terceiro = (segundo,primeiro)
 | (terceiro < primeiro && terceiro < segundo) && primeiro < segundo = (terceiro,segundo)
 | (terceiro < primeiro && terceiro < segundo) && primeiro > segundo = (terceiro,primeiro)

ordenaTripla :: Int -> Int -> Int -> (Int,Int,Int) -- retorna a tupla com os 3 argumentos ordenados
ordenaTripla primeiro segundo terceiro
 | (primeiro < segundo && primeiro < terceiro) && segundo < terceiro = (primeiro,segundo,terceiro)
 | (primeiro < segundo && primeiro < terceiro) && segundo > terceiro = (primeiro,terceiro,segundo)
 | (segundo < primeiro && segundo < terceiro) && primeiro < terceiro = (segundo,primeiro,terceiro)
 | (segundo < primeiro && segundo < terceiro) && primeiro > terceiro = (segundo,terceiro,primeiro)
 | (terceiro < primeiro && terceiro < segundo) && primeiro < segundo = (terceiro,primeiro,segundo)
 | (terceiro < primeiro && terceiro < segundo) && primeiro > segundo = (terceiro,segundo,primeiro)

type Ponto = (Float,Float) 
primeiraCoordenada :: Ponto -> Float -- retorna a primeira cordenada de um ponto
primeiraCoordenada (x,y) = x
segundaCordenada :: Ponto -> Float -- retorna a segunda cordenada de um ponto
segundaCordenada (x,y) = y

vertical :: Ponto -> Ponto -> Bool
vertical primeiro segundo 
 | primeiraCoordenada (primeiro) == primeiraCoordenada (segundo) = True
 | otherwise = False

 



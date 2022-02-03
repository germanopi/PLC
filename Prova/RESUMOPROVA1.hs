--1) DERIVAR MAP  
map       -- função -> lista -> lista (a -> b) -> [a] -> [b] Aplica  função na lista
.         --                          (b -> c) -> (a -> b) -> a -> c 
map . (.) -- função -> lista -> lista (b -> c) -> [a -> b] -> [a -> c]

-- 2) SUBLISTAS 
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:ys | ys <- sublistas xs] ++ sublistas xs -- Compreensão de Listas ( Definir listas em função de outra lista )

-- 3)a) POLINOMIAL
poli :: Int -> Int -> Int -> Int -> Int
poli a b c = (\x -> a * x * x + b * x + c) -- Função anonima aguarda x e aplica no corpo
-- 3)b) 
listaPoli :: [(Int, Int, Int)] -> [Int -> Int]
listaPoli l = [ poli a b c | (a,b,c) <- l ]
-- 3)c)
appListaPoli :: [Int -> Int] -> [Int] -> [Int]
appListaPoli a b = [f x | f <- a, x <- b]

-- 4) ALTMAP 
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g l = aplica f g l 1 (length l)

aplica f g [] n tam = []
aplica f g (x:xs) n tam
   | (mod n 2 == 0 ) && ( n <= tam ) = g x : aplica f g xs (n+1) tam
   | (mod n 2 /= 0 ) && ( n <= tam ) = f x : aplica f g xs (n+1) tam

-- 5) MOBILE
data Mobile = Pendente Int | Barra Mobile Mobile

peso :: Mobile −> Int
peso ( Pendente n ) = n
peso ( Barra m1 m2 ) = peso m1 + peso m2 -- Recursão

balanceado :: Mobile −> Bool
balanceado ( Pendente _ ) = True
balanceado ( Barra m1 m2) = peso m1 == peso m2 && balanceado m1 && balanceado m2

instance Eq Mobile where -- Não faz parte da questão
   (Pendente n1) == (Pendente n2) = n1 == n2 
   (Barra n1 n2) == (Barra m1 m2) = n1 == m1 && n2 == m2

-- 6) SUCESSÃO
compreensão :: [Int] -> [Int]
compreensão (x:xs) = [ x | (x,y) <- zip (x:xs) xs, x == y ]
recursão :: [Int] -> [Int]
recursão [x] = []
recursão (x:y:zs) 
    | x==y = x : recursão (y:zs)
    | otherwise = recursão (y:zs)

-- 7) MAP,FILTER E FOLDR
ehPar :: [Int] -> Bool
ehPar p = foldr (&&) True (map (\y -> mod y 2 == 0) (filter (\x -> 0 <= x && x <= 100) p))
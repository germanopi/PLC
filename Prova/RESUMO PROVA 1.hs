
take n [] -- retorna os n primeiros elementos da lista
drop n [] -- retorna todos os elementos da lista , exceto os n primeiros 
init [] -- retorna todos os elementos da lista exceto o ultimo

map dobro [2,3] = [4,6] -- Map (Aplica uma função a uma lista)
filter (>2) [2,3] = [3] -- filter (retorna elementos da lista aceitos na condição)
foldr1 (*) [2,3] = 6--foldr1 (aplica a operação na lista)

-- Compreensão de Listas ( Definir listas em função de outra lista )

dobrarLista :: [Int] -> [Int] 
dobrarLista l = [ 2*x | x <- l] -- dobra o elemento x da lista L, se x está em L

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
 

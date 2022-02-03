data Lista = Nill | Construtor Int Lista deriving (Show) -- Vazio ou Construtor da lista de Inteiros 

somaLista :: Lista -> Int
somaLista Nill = 0 
somaLista (Cons n l ) = n + somaLista l

{-
somaLista (Construtor 1 (Construtor 2 Nill)) = 1 + somaLista (Construtor 2 Nill)
somaLista (Construtor 1 (Construtor 2 Nill)) = 1 + 2 + somaLista Nill
somaLista (Construtor 1 (Construtor 2 Nill)) = 1 + 2 + 0
somaLista (Construtor 1 (Construtor 2 Nill)) = 3
-}

data Arvore = Nill |  No Int Arvore Arvore  deriving (Show)  -- Vazio ou Nó de Inteiros a esquerda e a direita 

{-
(No 3 (No 2 Nill Nill) (No 5 Nill Nill)) =       3
                                             /      \
                                            2        5
                                           /\        /\
                                       Nill Nill Nill Nill
-}

somaArvore :: Arvore -> Int -- soma o valor do nó com o nó da arvore a esquerda e a direita 
somaArvore Nill = 0 
somaArvore (No val arvE arvD) = val + somaArvore arvE + somaArvore arvD 

{- 
somaArvoreInt (No 1 (No 2 Nill Nill) Nill) = 1 + somaArvore (No 2 Nill Nill) + somaArvore Nill
somaArvoreInt (No 1 (No 2 Nill Nill) Nill) = 1 + 2 + somaArvore Nill + somaArvore Nill + soma Arvore Nill
somaArvoreInt (No 1 (No 2 Nill Nill) Nill) = 1 + 2 + 0 + 0 + 0
somaArvoreInt (No 1 (No 2 Nill Nill) Nill) = 3 
-}

maxi :: Int -> Int -> Int
maxi x y 
 | x >= y = x 
 | otherwise = y

profundidade :: Arvore -> Int -- calcula a profundidade da arvore
profundidade Nill = 0 
profundidade (No _ ae ad ) 1 + maxi (profundidade ae) (profundidade ad) 

collapse :: Arvore -> [Int] -- retorna os nós da arvore em busca por profundidade 
collapse Nill =[]
collapse (No val ae ad) = collapse ae ++ [val] ++ collapse ad

-- Tipos polimorfios 

-- Polimorfia uma mesma função pode ser usada para diferentes tipos, sem sofrer alteração alguma na seua definição/Corpo

data ListaPolimorfia t  = NillP | ConstrutorP t (ListaPolimorfia t) deriving (Show)
data ArvorePolimorfia t = NillP | No t (ArvorePolimorfia t) (ArvorePolimorfia t) deriving (Show)

membroLista :: [t] -> t -> Bool
membroLista [] b = False 
membroLista (a:as) b = (a==b) || membroLista as b 

-- classes Conjunto de tipos para qual uma função está definida , Permite sobrecarga (Função pode ser usada para diferentes tipos mudando apenas a definição/Corpo da função )

deriving ( Show , Eq , Ord ) -- Chama as classes Show, Eq (Conjunto de tipos que "==" está definida) e Ord (Conjunto de tipos que ">,<,>=,<=" está definida)

class Visivel t where  -- criação da classe visivel
    toString :: t -> String
    size :: t -> Int

-- Instancias/Objetos da Classe (Tipos membros da classe)

instance Visivel Bool where  -- instancia / objeto da classe visivel 
    toString True = "True"
    toString False = "False"
    size _ = 1 

instance Visivel Curso where
    toString (Manha n)= "Curso" ++ n ++ " da manhã"
    toString (Tarde n)= "Curso" ++ n ++ " da tarde"
    size _ = 1 

instance Show Curso where  -- classe Show é padrão
    toString (Manha n)= "Curso pela Manha " ++ n 
    toString (Tarde n)= "Curso pela tarde" ++ n 
   
instance visivel a => Visivel [a] where  -- prof n mostrou tudo
    toString = concat . (map toString)
    size =   (foldr (+) 0) . (map size)

---------- ASSUNTO DA PROVA 1 TERMINA AKI --------------------

-- dica de dogra map, filter , folder, compresão de lista cai na prova 
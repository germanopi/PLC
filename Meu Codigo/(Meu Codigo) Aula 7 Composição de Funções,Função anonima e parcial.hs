-- Composição de funções 

(f . g) x = f (g x) -- f(g(x)) aplica x em g , aplica o retorno de g em f 

-- ( f . g ) x realiza primeiro a operação a direita do . usando x, e em seguida a operação a esquerda usando o retorno da função a direita

-- A função de composição  ( f . g ) x é do tipo ( b -> c) -> ( a -> b ) que resulta em ( a -> c )

inc :: Int ->Int
inc x = x + 1

aplicaDuasVezes :: (t->t) -> t ->t  
aplicaDuasVezes f x = f ( f x ) -- metodo antigo 
aplicaDuasVezes f x = (f.f) x -- novo metodo

{-
aplicaDuasVezes inc 3 =  (inc . inc ) 3   aplicação da funcao aplicaDuasVezes
aplicaDuasVezes inc 3 =  inc ( inc ) 3    definicao da funcao 
aplicaDuasVezes inc 3 =  inc 4      
aplicaDuasVezes inc 3 =   5
-}

itter :: Int -> (t->t)-> (t->t)
itter 0 f = id
itter n f = (itter ( n - 1 ) f ) . f

{-
itter 3 inc = (itter 2 inc) . inc 
itter 3 inc = ((itter 1 inc) . inc ) . inc 
itter 3 inc = (((itter 0 inc) . inc ) . inc ) . inc 
itter 3 inc = ((id .  inc) . inc ) . inc 
-}

incN :: Int-> Int -> Int
incN n x = (itter n inc) x 

-- notação lambda para função anonima.  (\x y -> x + y + 1) 2 3 retorna 6, a função anonima \  aguarda x e y e retorna  x + y + 1 
--  f . g = \x -> f (g x) 

addNum :: Int->(Int -> Int) -- (addNum  2) 4 retorna 6
addNum n = h 
 where 
      h x = n + x 

addNumNotaçãoLambda :: Int->(Int -> Int)  
addNumNotaçãoLambda n = (\x -> m + x ) -- a função anonima \ aguarda x e retorna m + x

-- \x y -> g (f x)(f' y) aguarda x e y ,  plica y em f', aplica x em f, aplica o retorno de f' e f em g 

--  Aplicação Parcial ,  função criada ao receber apenas uma parte dos argumentos requisitados 

mult2Int :: Int->Int -> Int -- (mult2Int 2 ) [1..6] retorna [2,4,6,8,10,12]
mult2Int x y = x * y 

-- f a b = (f a) b  aplicar a em f e depois aplicar b 

-- (>6) 7 retorna True
-- (/2) 2000 retorna 1000

-- itter 3 (/2) 2000 /= itter 3 ((/) 2) 2000

{-
itter 3 (/2) = (itter 2 (/2)) . (/2)
itter 3 (/2) = ((itter 1 (/2)) . (/2))  . (/2)
itter 3 (/2) = ((((itter 0 (/2)). (/2)) . (/2)) . (/2)
itter 3 (/2) =  ((id . (/2)) . (/2)) . (/2)
-}  

{-
iter 3 ((/) 2)) = ((iter 2 ((/) 2))) . ((/) 2)))  
iter 3 ((/) 2)) = (((iter 1 ((/) 2))). ((/) 2))) . ((/) 2)))  
iter 3 ((/) 2)) = (((iter 0 ((/) 2))) . ((/) 2))). ((/) 2))) . ((/) 2)))  
iter 3 ((/) 2)) = ((id) . ((/) 2))). ((/) 2))) . ((/) 2)))  
-}

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z)

{-
allEqual 1 2 3 = (allEqual 1) 2 3
allEqual 1 2 3 = ((allEqual 1) 2) 3
-}







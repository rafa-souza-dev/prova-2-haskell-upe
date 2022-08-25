module Prova where


-- exec 1
f :: Int -> Int
f n = 2 * n

duasVezes :: (t -> t) -> t -> t
duasVezes f x = f (f x)

-- exec 2
terceiroError = error "a lista precisa conter pelo menos 3 elementos"

terceiro1 :: [a] -> a
terceiro1 lista
    | length lista < 3 = terceiroError 
    | otherwise = head (tail (tail lista))

terceiro2 :: [a] -> a
terceiro2 lista
    | length lista < 3 = terceiroError
    | otherwise = lista !! 2

terceiro3 :: [a] -> a
terceiro3 [] = terceiroError
terceiro3 [x] = terceiroError
terceiro3 [x, y] = terceiroError
terceiro3 lista = lista !! 2

-- exec 3
minhaTail1 :: [a] -> [a]
minhaTail1 lista = if length lista == 0 then [] else drop 1 lista

minhaTail2 :: [a] -> [a]
minhaTail2 lista
    | length lista == 0 = []
    | otherwise         = drop 1 lista

minhaTail3 :: [a] -> [a]
minhaTail3 [] = []
minhaTail3 lista = drop 1 lista

-- exec 4
reverso :: [a] -> [a]
reverso [] = []
reverso lista = lista !! ultimo_indice : reverso (take ultimo_indice lista) where
    ultimo_indice = (length lista) - 1

-- exec 5
xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor True False = True
xor False True = True

divisivel :: Int -> [Int] -> Bool
divisivel _ [] = error "A lista não pode ser vazia"
divisivel n lista = if length [x | x <- lista_sem_1_e_n, mod n x == 0] > 0 then True else False where
    lista_sem_1_e_n = take (length lista - 2) (drop 1 lista)

primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo n = if xor ((mod n 1 == 0) && (mod n n == 0)) (divisivel n [1..n]) then True else False

primos :: Int -> [Int]
primos 0 = []
primos 1 = []
primos n = [x | x <- [1..n], primo x]

-- exec 6
enviaAoFinal :: [a] -> [a]
enviaAoFinal [] = []
enviaAoFinal [x] = [x]
enviaAoFinal lista = drop 1 (lista ++ [lista !! 0])

shiftLeft :: Int -> [a] -> [a]
shiftLeft 0 lista = lista
shiftLeft _ [] = []
shiftLeft n lista = shiftLeft (n-1) (enviaAoFinal lista)

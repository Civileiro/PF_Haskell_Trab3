--Nome: Gabriel Prost Gomes Pereira

import Data.List

formatExample :: [Char] -> [Char] -> [Char] -> [Char]
formatExample n arg result = "Func." ++ n ++ ":entrada:" ++ arg ++ ":resultado:" ++ result

showExample :: (Show a, Show b, Show c) => a -> [b] -> c -> [Char]
showExample n arg result = formatExample nS argS resultS
  where
  nS = show n
  argS = [x | x <- unwords (map show arg), not (elem x "'\"")]
  resultS = show result


main = do
  print (showExample 1 [1] (fib 1))
  print (showExample 1 [3] (fib 3))
  print (showExample 1 [5] (fib 5))
  print (showExample 1 [9] (fib 9))
  print (showExample 2 [30, 9] (mdc 30 9))
  print (showExample 2 [100, 70] (mdc 100 70))
  print (showExample 2 [-81, 9] (mdc (-81) 9))
  print (showExample 2 [-4, -18] (mdc (-4) (-18)))
  print (showExample 3 [1234] (sumDigitos 1234))
  print (showExample 3 [0] (sumDigitos 0))
  print (showExample 3 [999999999] (sumDigitos 999999999))
  print (showExample 3 [-55] (sumDigitos (-55)))
  print (showExample 4 "" sumMult)
  print (showExample 5 [[1, 3, 5]] (diffSums [1, 3, 5]))
  print (showExample 5 [[-10, 3, 7]] (diffSums [-10, 3, 7]))
  print (showExample 5 [[5, -3, 13]] (diffSums [5, -3, 13]))
  print (showExample 6 [3] (eulersSieve 3))
  print (showExample 6 [5] (eulersSieve 5))
  print (showExample 6 [10] (eulersSieve 10))
  print (showExample 6 [100] (eulersSieve 100))
  print (showExample 6 [1000] (eulersSieve 1000))
  print (showExample 7 [20] (lucasLessThan 20))
  print (showExample 7 [150] (lucasLessThan 150))
  print (showExample 7 [4] (lucasLessThan 4))
  print (showExample 7 [3] (lucasLessThan 3))
  print (showExample 7 [2] (lucasLessThan 2))
  print (showExample 7 [-1] (lucasLessThan (-1)))
  print (showExample 8 [[1, 2, 3]] (aoContrario [1, 2, 3]))
  print (showExample 8 ["[]"] (aoContrario []))
  print (showExample 8 [[-1, -5, 999]] (aoContrario [-1, -5, 999]))
  print (showExample 8 ["[5..20]"] (aoContrario [5..20]))
  print (showExample 9 [7, 8] (somaRecursiva 7 8))
  print (showExample 9 [-10, 81] (somaRecursiva (-10) 81))
  print (showExample 9 [13, -5] (somaRecursiva 13 (-5)))
  print (showExample 9 [-76, -1] (somaRecursiva (-76) (-1)))
  print (showExample 10 ["[]"] (comprimento []))
  print (showExample 10 [[1, 2, 3]] (comprimento [1, 2, 3]))
  print (showExample 10 ["[1..1000]"] (comprimento [1..1000]))

{-
1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci, utilizando Haskell.
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

{-
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. 
-}
mdc :: Integer -> Integer -> Integer 
mdc a 0 = abs a
mdc a b = mdc b (rem a b)

{-
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade. 
-}
toDigitList :: Integer -> [Integer]
toDigitList 0 = []
toDigitList x = toDigitList (div x 10) ++ [rem x 10]

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumDigitos :: Integer -> Integer
sumDigitos x = sumList (toDigitList (abs x))

{-
4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5. 
-}
divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = rem y x == 0

divisibleByAnyInteger :: [Integer] -> Integer -> Bool
divisibleByAnyInteger [] y = False
divisibleByAnyInteger (x:xs) y = divisibleBy x y || divisibleByAnyInteger xs y

sumMult :: Integer
sumMult = sum [x | x <- [1..9999], divisibleByAnyInteger [3, 5] x]

{-
5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
-}
sumSquares :: [Integer] -> Integer
sumSquares [] = 0
sumSquares (x:xs) = x^2 + sumSquares xs 

squareSums :: [Integer] -> Integer -> Integer
squareSums [] s = s^2
squareSums (x:xs) s = squareSums xs (s + x)

diffSums :: [Integer] -> Integer
diffSums x = sumSquares x - squareSums x 0
  -- where
  -- sumSquares = sum[a^2 | a <- x]
  -- squareSums = (sum x)^2

{-
6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.  
-}
_eulersSieve :: [Integer] -> Integer
_eulersSieve [] = 0
_eulersSieve [p] = 1
_eulersSieve (p:t) = n
  where
  multList = map (*p) (p:t)
  (_:newList) = (p:t) \\ multList
  n = 1 + _eulersSieve newList

eulersSieve :: Integer -> Integer
eulersSieve n = _eulersSieve [2..(n - 1)]

{-
7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
-}
lucasList :: Integer -> [Integer] -> [Integer]
lucasList x [] 
  | x > 2 = lucasList x [2]
  | otherwise = []
lucasList x [n] = lucasList x [1, n]
lucasList x (n1:n2:t) 
  | x > n1+n2 = lucasList x (n1+n2:n1:n2:t)
  | otherwise = (n1:n2:t)
    

lucasLessThan :: Integer -> [Integer]
lucasLessThan n = reverse (lucasList n []) 

{-
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
-}
aoContrario :: [Integer] -> [Integer]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

{-
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
-}

_somaRecursiva :: Integer -> Integer -> Integer
_somaRecursiva x 0 = 0
_somaRecursiva x y = x + _somaRecursiva x (y - 1)

somaRecursiva :: Integer -> Integer -> Integer
somaRecursiva x y 
  | y < 0 = _somaRecursiva (-x) (-y)
  | otherwise = _somaRecursiva x y

-- somaRecursiva :: Integer -> Integer -> Integer
-- somaRecursiva x y 
--   | x < 0 = somaRecursiva (-x) (-y)
--   | otherwise = sum (replicate x y)

{-
10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
-}
comprimento :: [Integer] -> Integer
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs
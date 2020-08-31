@Autor: Marcos Rodrigues
@Matrícula: 14.2.4341

=== MATERIAL DE APOIO ===
https://github.com/rodrigogribeiro/tutoria-bcc222/blob/master/semana1/app/Semana1.pdf

Removendo funções que serão implementadas manualmente
\begin{code}
import Prelude hiding (gcd)
\end{code}

==  RECURSÃO SOBRE INTEIROS ==
=   EXERCÍCIO 1     =

a) codifique a função:
\begin{code}
next :: Int -> Int
next n
    | n `mod` 2 == 0    =   (n `div` 2)
    | otherwise         =   3 * n + 1
\end{code}

b) usando 'next', implemente 'steps':
\begin{code}
steps :: Int -> [Int]
steps n 
    | n == 1    =   [1]
    | otherwise =   n : steps (next n)
\end{code}

c) implemente stepCounter
\begin{code}
stepsCounter :: Int -> (Int, Int)
stepsCounter n = stepsCounter' (n, 0)

stepsCounter' :: (Int, Int) -> (Int, Int)
stepsCounter' (n, x)
    | n == 1     =  (1, 1)
    | otherwise  =  stepsCounter' (next n, x + 1) 
\end{code}

d) implmente stepList
\begin{code}
stepsList :: Int -> ([Int], Int)
stepsList n = stepsList' ([], n, 0)
    
stepsList' :: ([Int], Int, Int) -> ([Int], Int)
stepsList' (arr, n, c)
    | n == 1    =   (arr ++ [n], (c + 1))
    | otherwise =   stepsList'(arr ++ [n], (next n), (c + 1))
\end{code}

=   EXERCÍCIO 2     =
a) implemente o calculo para mdc
\begin{code}
gcd :: Int -> Int -> Int
gcd a b
    |   b == 0      =   a
    |   otherwise   =   gcd b (a `mod` b)
\end{code}

b) implemente o gcdCounter
\begin{code}
gcdCounter :: Int -> Int -> (Int, Int)
gcdCounter a b = counter a b 1
    where
        counter a b c
            |   b == 0      =   (a, c)
            |   otherwise   =   counter b (a `mod` b) (c + 1)
\end{code}

==  MÉTODO DE NEWTON    ==
=   EXERCÍCIO 1 =
Implemente average
\begin{code}
average :: Double -> Double -> Double
average a b = (a + b) / 2
\end{code}

=   EXERCÍCIO 2 =
Implemente goodEnough
\begin{code}
goodEnough :: Double -> Double -> Bool
goodEnough tent x 
    |   abs(tent ^ 2 - x) < 0.001  =   True
    |   otherwise               =   False
\end{code}

=   EXERCÍCIO 3 =
Implmente improve
\begin{code}
improve :: Double -> Double -> Double
improve tent rad = average tent (rad / tent)
\end{code}

=   EXERCÍCIO 4 =
Implemente sqrtIter
\begin{code}
sqrtIter :: Double -> Double -> Double
sqrtIter tent rad 
    |   goodEnough tent rad     =   tent
    |   otherwise               =   sqrtIter (improve tent rad) rad
\end{code}

=   EXERCÍCIO 5 =
Implemente cubicIter
\begin{code}
goodEnough' :: Double -> Double -> Bool
goodEnough' tent x 
    |   abs(tent ^ 2 - x) < 0.0001  =   True
    |   otherwise                   =   False

approximation :: Double -> Double -> Double
approximation x y = ((x / (y ^ 2)) - (2 * y)) / 3

cubicIter :: Double -> Double -> Double
cubicIter tent rad 
    |   goodEnough' tent rad        =   approximation rad tent
    |   otherwise                   =   cubicIter (improve tent rad) rad
\end{code}

==  LIST COMPREHENSIONS    ==
=   EXERCÍCIO 1 =
Implemente squareSum
\begin{code}
squareSum :: Int -> Int
squareSum n = sum [x ^ 2 | x <- [1..n]]
\end{code}

=   EXERCÍCIO 2 =
Implemente grid
\begin{code}
grid :: Int -> Int -> [(Int, Int)]
grid m n
    = [(x, y) | x <- [0..n],
                y <- [0..m]]
\end{code}

=   EXERCÍCIO 3 =
Iplmente scalarProduct
\begin{code}
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | x <- xs, y <- ys]
\end{code}
=== AULA 4  -   PARTE 1 ===
Subject: LIST COMPREHENSIONS

\begin{code}
import Prelude

ex1 :: Int
ex1 = sum [x ^ 2 | x <- [1..20], odd x]
\end{code}
toLowers :: String -> String
toLowers xs = [toLower c | c <- xs]

selectDigits :: String -> String
selectDigits xs = [c | c <- xs, isDigit c]

\begin{code}
(.*.) :: [a] -> [b] -> [(a, b)]
xs .*. ys = [(x, y) | x <- xs, y <- ys]

triples :: Int -> [(Int, Int, Int)]
triples n
    =   [(x, y, z) |   x <- [1..n],
                        y <- [1..n],
                        z <- [1..n],
                        (x ^ 2) == (y ^ 2) + (z ^ 2)]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
\end{code}

=== AULA 4  -   PARTE 3 ===
Subject: FUNÇÕES POLIMÓRFICAS
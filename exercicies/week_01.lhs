@Autor: Marcos Rodrigues
@Matrícula: 14.2.4341

=== MATERIAL DE APOIO ===
https://github.com/rodrigogribeiro/tutoria-bcc222/blob/master/semana1/app/Semana1.pdf

== EXERCICIOS ==

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
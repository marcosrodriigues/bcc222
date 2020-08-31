    =====   AULA 02 - PARTE 02  =====

Exemplo 1
==========
\begin{code}
firstOrEmpty :: [String] -> String
firstOrEmpty xs
   = if null xs then "empty"
   else head xs
\end{code}

Exemplo 2
==========

\begin{code}
size :: [Int] -> Int
size xs
    = if null xs then 0
    else 1 + size (tail xs)
\end{code}

Exemplo 3
==========
\begin{code}
(+++) :: [Int] -> [Int] -> [Int]
xs +++ ys
    = if null xs then ys
    else (head xs) : ((tail xs) +++ ys)
\end{code}

Exercicios
==========

1. Desenvolver uma função que inverte uma lista

\begin{code}
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x: xs) = ((inverte xs) +++ [x])
\end{code}

2. Desenvolver uma função que soma os elementos de uma lista de números inteiros

\begin{code}
soma :: [Int]   -> Int
soma []         = 0
soma xs         = head xs + soma (tail xs)
\end{code}


    =====   AULA 02 - PARTE 03  =====

\begin{code}
maxmin :: [Int] -> (Int, Int)
maxmin xs 
    = if null (tail xs) then (head xs, head xs)
    else ( 
        if (head xs) > fst (maxmin (tail xs))
            then head xs
            else fst (maxmin (tail xs))
        ,if (head xs) < snd (maxmin (tail xs))
            then head xs
            else snd (maxmin (tail xs))
    )
\end{code}

\begin{code}
maxmin' :: [Int] -> (Int, Int)
maxmin' xs
    =   if null tl then (h,h)
        else (
            if h > max_xs then h else max_xs,
            if h < min_xs then h else min_xs
        )
    where
        h = head xs
        tl = tail xs
        p = maxmin' (tail xs)
        max_xs = fst p
        min_xs = snd p
\end{code}
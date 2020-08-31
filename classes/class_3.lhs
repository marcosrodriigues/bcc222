=====      AULA 3 - PARTE 1    =====
Subject: TIPOS ALGÉBRICOS

\begin{code}
import Prelude
    hiding ( sum, length, (++), replicate, reverse)
    
type Name = String
type Surname = String
type SendOffer = Bool

type Cliente = (Name, Surname, SendOffer)

example :: Cliente
example = ("José", "Silva", False)
\end{code}

\begin{code}
data Client
    = Customer Name Surname SendOffer
    | Company Name

example_2 :: Client
example_2 = Customer "Jos" "Silva" False

example_3 :: Client
example_3 = Company "Distribuidora de Cerveja"
\end{code}

=====      AULA 3 - PARTE 2    =====
Subject: CASAMENTO DE PADRÃO

\begin{code}
greet :: Client -> String
greet (Customer nm sm so) = "Bem vindo, " ++ nm
greet (Company nm) = "Bem vindo, " ++ nm
\end{code}

=====      AULA 3 - PARTE 3    =====
Subject: FUNÇÕES RECURSIVAS COMBINANDO CASAMENTO DE PADRÃO E RECURSÃO

\begin{code}

sum :: [Int] -> Int
sum []      = 0
sum (x: xs) = x + sum xs

length :: [a] -> Int
length []       = 0
length (_ : xs)  = 1 + length xs
\end{code}

=====      AULA 3 - PARTE 4    =====
\begin{code}
(++) :: [a] -> [a] -> [a]
[]      ++ ys = ys
(x: xs) ++ ys = x : (xs ++ ys)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n v   = v : replicate (n-1) v
\end{code}

=====      AULA 3 - PARTE 5    =====
\begin{code}
reverse :: [a] -> [a]
reverse []      = []
reverse (x: xs) = (reverse xs) ++ [x]

reverse' :: [a] -> [a]
reverse' xs = rev xs []
    where
        rev []          ac  = ac
        rev (x : xs)    ac  = rev xs (x : ac) 
\end{code}

=====      AULA 3 - PARTE 6    =====
Subject: INSERTION SORT

\begin{code}
insert :: Int -> [Int] -> [Int]
insert x []         = [x]
insert x (y : ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys 

isort :: [Int] -> [Int]
isort []        = []
isort (x: xs)   = insert x (isort xs)
\end{code}

=====      AULA 3 - PARTE 7    =====
Subject: REGISTROS

\begin{code}
data RPerson    =   RPerson Name Surname

rname :: RPerson -> Name
rname (RPerson nm _) = nm

rsurname :: RPerson -> Surname
rsurname (RPerson _ sm) = sm

data Person = Person {
    name    ::  Name,
    surname ::  Surname
}

ex1 :: Person
ex1 = Person { 
    name    =   "Marcos",
    surname =   "Rodrigues"    
}

greet' :: Person -> String
greet' p = "Bem vindo, " ++ name p ++ "!"

\end{code}
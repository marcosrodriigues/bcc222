=== AULA 4  -   PARTE 1 ===
Subject: LIST COMPREHENSIONS

\begin{code}
import Prelude hiding (map, filter, curry, uncurry,
                        sum, and, concat, foldr, takeWhile, all, (.),
                        zip, repeat, take, iterate, drop)
import Data.Char (isLower, ord, chr)

example_1 :: Int
example_1 = sum [x ^ 2 | x <- [1..20], odd x]
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

\begin{code}
maybeString :: Maybe a -> String
maybeString (Just _)    = "Just"
maybeString Nothing     = "Nothing"

type Name       = String
type Surname    = String

data Person
    = Person {
        name    ::  Name,
        surname ::  Surname
    } deriving Show

data Client i
    = Company {
        companyId   ::  i,
        companyName ::  Name,
        contact     ::  Person
    }
    | Individual {
        individualId    ::  i,
        person          ::  Person
    } deriving Show

ex1 :: Client String
ex1 = Company {
    companyId   =   "DistMaria",
    companyName =   "Distribuidora da Maria",
    contact     =
        Person {
            name    =   "Maria",
            surname =   "Silva"
        }
}

ex2 :: Client Integer
ex2 = Company {
    companyId   =   1,
    companyName =   "Distribuidora da Maria",
    contact     =
        Person {
            name    =   "Mari a",
            surname =   "Silva"
        }
}
\end{code}

=== AULA 4  -   PARTE 4 ===
Subject: FUNÇÕES DE ORDEM SUPERIOR

\begin{code}
doubleList :: [Int] -> [Int]
doubleList []       =   []
doubleList (x : xs) =   2 * x : doubleList xs

notList :: [Bool] -> [Bool]
notList []          =   []
notList (x : xs)    =   not x : notList xs

map :: (a -> b) -> [a] -> [b]
map _ []        =   []
map f (x : xs)  =   f x : map f xs

doubleList' xs 
    = map double xs
        where
            double x = 2 * x

notList' xs = map not xs
\end{code}

=== AULA 4  -   PARTE 5 ===
Subject: FUNÇÕES EVENS / FILTER

\begin{code}
evens :: [Int] -> [Int]
evens []        =   []
evens (x : xs) 
    | even x        = x : evens xs
    | otherwise     =   evens xs

lowers :: String -> String
lowers []       =   []
lowers (x : xs) 
    |   isLower x   =   x : lowers xs
    |   otherwise   =   lowers xs

filter :: (a -> Bool) -> [a] -> [a]
filter  _ []        =   []
filter p (x : xs)   
    | p x       =   x : filter p xs
    | otherwise =   filter p xs

evens'  = filter even
lowers' =   filter isLower
\end{code}

=== AULA 4  -   PARTE 6 ===
Subject: FUNÇÕES PARA MUDAR A ORDEM DE PARAMETROS
         APLICAÇÃO PARCIAL

\begin{code}
flip :: (a -> b -> c) -> b -> a -> c
flip f y x  = f x y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

($) :: (a -> b) -> a -> b
f $ x   =   f x

doubleAll :: [Int] -> [Int]
doubleAll xs    =   map double xs
    where double x = 2 * x

doubleAll' :: [Int] -> [Int]
doubleAll' xs    =   map (\ x -> 2 * x) xs

add3 :: Int -> Int -> Int -> Int
add3    =   \ x y z ->  x + y + z

test = add3 2 2
\end{code}

=== AULA 4  -   PARTE 7 ===
Subject: COMPOSIÇÃO

\begin{code}
applyAll :: [a -> a] -> a -> a
applyAll [] x       =   x
applyAll (f : fs) x =   applyAll fs (f x) 

(.) :: (b -> c) -> (a -> b) -> a -> c
g . f   =   \ x -> g (f x)

applyAll' :: [a -> a] -> a -> a
applyAll'   []          =   id
applyAll'   (f : fs)    =   applyAll' fs . f
\end{code}

=== AULA 4  -   PARTE 8 ===
Subject: FOLDR

\begin{code}
sum :: [Int] -> Int
sum []          =   0
sum (x : xs)    = x + sum xs

and :: [Bool] -> Bool
and []          = True
and (x : xs) = x && and xs

concat :: [[a]] -> [a]
concat []           = []
concat (xs : xss)   = xs ++ concat xss

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr   _ v []      = v
foldr f v (x : xs)  = f x (foldr f v xs)

sum' :: [Int] -> Int
sum' = foldr (+) 0

and' :: [Bool] -> Bool
and' = foldr (&&) True

concat' :: [[a]] -> [a]
concat' = foldr (++) []
\end{code}

=== AULA 4  -   PARTE 9 ===
Subject: FOLDR - CONTINUAÇÃO

\begin{code}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr step []
    where
        step x ac = f x : ac

length' :: [a] -> Int
length' = foldr (\ _ ac -> 1 + ac) 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr step []
    where
        step x ac = if p x then x : ac
                    else ac

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr step []
    where
        step x ac = if p x then x : ac 
                    else []
\end{code}

=== AULA 4  -   PARTE 10 ===
Subject: SERIALIZAÇÃO

\begin{code}
take :: Int -> [a] -> [a]
take 0 _        = []
take n []       = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs       = xs
drop n (_ : xs) =   drop (n - 1) xs
drop _ []       = error "drop: Empty list!"

zip :: [a] -> [b] -> [(a, b)]
zip [] _                = []
zip _ []                = []
zip (x : xs) (y : ys)   = (x, y): zip xs ys

repeat :: a -> [a]
repeat x = x : repeat x

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

type Bit    =   Int

bin2Int :: [Bit] -> Int
bin2Int bs =  sum [w * b | (w, b) <- zip weights bs]
    where
        weights = iterate (* 2) 1

int2Bin :: Int -> [Bit]
int2Bin 0 = []
int2Bin n = n `mod` 2 : int2Bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

encode :: String -> [Bit]
encode = concat . map (make8 . int2Bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2Int) . chop8
\end{code}


=== AULA 4  -   ESTUDO DE CASO  ===
Subject: CIFRA DE CESAR

\begin{code}
char2Int :: Char -> Int
char2Int c = ord c - ord 'a'

int2Char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
    | isLower c     =   int2Char (m `mod` 26)
    | otherwise     =   c
    where m = char2Int c + n

encode' :: Int -> String -> String
encode' n = map (shift n)

decode' :: Int -> String -> String
decode' n = encode' (- n)
\end{code}
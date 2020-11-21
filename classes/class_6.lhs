---
    AULA 6 - PARTE 1
    CLASSES DE TIPOS
---

> data Tree a
>   =   Leaf
>   |   Node a (Tree a) (Tree a)
>   deriving Show

> insert1 :: (a -> a -> Bool) ->
>            (a -> a -> Bool) ->
>            a                ->
>           Tree a            ->
>           Tree a
> insert1 lt eq v Leaf = Node v Leaf Leaf
> insert1 lt eq v (Node v' l r)
>   | eq v v'   = Node v' l r
>   | lt v v'   = Node v' (insert1 lt eq v l) r
>   | otherwise = Node v' l (insert1 lt eq v l)

> data LtEq a 
>   = LtEq {
>       lt :: a -> a -> Bool,    -- menor, < 
>       eq :: a -> a -> Bool    -- igualdade, == 
>   }

> insert2 :: LtEq a ->
>            a      ->
>            Tree a ->
>            Tree a 
> insert2 lteq v Leaf = Node v Leaf Leaf
> insert2 lteq v (Node v' l r)
>   | eq lteq v'   = Node v' l r
>   | lt lteq v'   = Node v' (insert2 lteq v l) r
>   | otherwise = Node v' l (insert2 lteq v l)


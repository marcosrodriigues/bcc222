---
    === AULA 5 - PARTE 1    ===
    ÁRVORE BINÁRIA DE BUSCA
---

> data Tree a
>   =   Leaf
>   | Node a (Tree a) (Tree a)
>   deriving (Eq, Ord, Show)

Interface de manipulação
-------------------------

1. Criar uma árvore vazia

> empty :: Tree a
> empty = Leaf

2. Inserir um valor
Entrada: comparação, valor a ser inserido, arvore pra Inserir
Saida: árvore 

> insert :: (a -> a -> Bool) -> a -> Tree a -> Tree a
> insert f v Leaf           = Node v empty empty
> insert f v (Node v' l r) 
>        |   f v v'      = Node v' (insert f v l) r
>        |   otherwise   = Node v' l (insert f v r)

> testInsert :: Tree Int
> testInsert = insert (<=) 4 (insert (<=) 1 (insert (<=) 2 empty))

---
    === AULA 5 - PARTE 2    ===
    ÁRVORE BINÁRIA DE BUSCA
---

3. Busca de elementos.
Entrada: Comparação, igualdade, valor a ser producrado, arvore pra procurar
Saída: Boolean (exists or not)

> search :: (a -> a -> Bool) -> (a -> a -> Bool) -> a -> Tree a -> Bool
> search _ _ v Leaf = False
> search lt eq v (Node v' l r)
>   | eq v v' = True
>   | lt v v' = search lt eq v l
>   | otherwise = search lt eq v r

---
    === AULA 5 - PARTE 3    ===
    ÁRVORE BINÁRIA DE BUSCA
---

4. Remoção de elementos.
Entrada: Comparação <, Comparação ==, valor a ser removido, arvore para buscar
Saida: arvore sem o elemento

> remove :: (a -> a -> Bool) -> (a -> a -> Bool) -> a -> Tree a -> Tree a
> remove _ _ _ Leaf  =   Leaf
> remove lt eq v (Node v' l r)
>   | eq v v'   = removeEq l r
>   | lt v v'   = Node v' (remove lt eq v l) r
>   | otherwise = Node v' l  (remove lt eq v r)

> removeEq :: Tree a -> Tree a -> Tree a 
> removeEq Leaf r = r
> removeEq l Leaf = l
> removeEq l r      -- data Maybe a = Nothing | Just a
>   = case removeMin l of
>       Nothing         ->  error "Impossible!"
>       Just (x, r')    -> Node x l r'


> removeMin :: Tree a -> Maybe (a, Tree a)
> removeMin Leaf = Nothing
> removeMin (Node v Leaf r) = Just (v, r)
> removeMin (Node v l r)
>   =   case removeMin l of
>       Nothing ->  Nothing
>       Just (v', l') -> Just (v', Node v l' r)

---
    === AULA 5 - PARTE 4    ===
    ÁRVORE BINÁRIA DE BUSCA
---

5. Conversão de uma lista para uma árvore
Entrada: Comparação (<), Comparação (>), Lista
Saída: árvore

> fromList :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> Tree a
> fromList lt eq = foldr (insert lt eq) Leaf

6. Conversão de uma árvore em uma lista

> toList :: Tree a -> [a]
> toList Leaf           = []
> toList (Node v l r)   = v : (toList l ++ toList r)

> toList' :: Tree a -> [a]
> toList' = flip toList1 []
>   where 
>       toList1 Leaf ac         = ac 
>       toList (Node v l r) ac  = v : toList1 l (toList' r ac)

---
    === AULA 5 - PARTE 5    ===
    ÁRVORE BINÁRIA DE BUSCA
    Funções de ordem superior
---

7. Função foldr
Entrada: funcao para combinar os resultados, retorno para Leaf, árvore
Saída: Resultado

> foldrTree :: (a -> b -> b -> b) -> b -> Tree a -> b
> foldrTree _ v Leaf            =   v
> foldrTree f v (Node v' l r)   =   f v' (foldrTree f v l) (foldrTree f v r) 

8. Função map

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f = 
>   foldrTree step Leaf
>   where
>       step v acl acr = Node (f v) acl acr

9. Função filter

> filterTree :: (a -> a -> Bool) -> (a -> a -> Bool) -> (a -> Bool) -> Tree a -> Tree b
> filterTree lt eq p = fromList lt eq . filter p . toList

Exercícios
----------

1. Reimplementar Search com foldrTree
2. Implementar calcular altura da árvore usando foldrTree
3. Implmntar calcular o numero de elementos de uma arvore usando foldrTree
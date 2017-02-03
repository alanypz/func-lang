-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #2
-- Written by Alan Yepez

module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a) 
                deriving (Eq, Show)

depth :: Tree a -> Integer
depth Nil            = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-- collapses a tree into a list by visiting 
-- the elements of the tree 'inorder'

collapse :: Tree a -> [a]
collapse Nil            = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2

-- stratifies a tree into a list by visiting
-- all elements at depth 1, then all elements 2, etc.

stratify :: Tree a -> [a]
stratify tree = stratify' [tree]

stratify' :: [Tree a] -> [a]
stratify' [] = []
stratify' tree = getnode ++ stratify' getchildren
   where getnode = [ x | Node x _ _ <- tree]
         getchildren = [ y | Node _ left right<-tree, y <- [left, right] ]

{-
	First solution implementation.

stratify :: Tree a -> [a]
stratify tree = stratify' [tree]

stratify' :: [Tree a] -> [a]
stratify' [] = []
stratify' tree = getnode ++ stratify' getchildren
   where getnode = [ x | Node x _ _ <- tree]
         getchildren = [ y | Node _ left right<-tree, y <- [left, right] ]

-}

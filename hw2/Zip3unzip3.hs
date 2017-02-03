-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #2
-- Written by Alan Yepez

module Zip3unzip3 where
import Prelude hiding (zip3, unzip3)

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _ _ _ = [] 

unzip3 :: [(a,b,c)] -> ([a], [b], [c])
unzip3 = foldr (\(x, y, z) (xs, ys, zs) -> (x:xs, y:ys, z:zs)) ([],[],[])

{-
  First implementation solution.

unzip3 :: [(a,b,c)] -> ([a], [b], [c])
unzip3 list = (lista, listb, listc)
  where lista = [a | (a,_,_)<-list]
        listb = [b | (_,b,_)<-list]
        listc = [c | (_,_,c)<-list]
-}
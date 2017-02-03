-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #2
-- Written by Alan Yepez

module Folds where


count :: Eq a => a -> [a] -> Integer
count a = foldl (\f x -> if x == a then (f + 1) else f) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 e = foldr (\x f -> if x == e then True else f) False
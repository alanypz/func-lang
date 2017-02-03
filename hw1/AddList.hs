-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #1
-- Written by Alan Yepez

module AddList where

add_list_comprehension :: Integer -> [Integer] -> [Integer]
add_list_comprehension num [] = []
add_list_comprehension num list = [i + num | i <- list]

add_list_recursion :: Integer -> [Integer] -> [Integer]
add_list_recursion num [] = []
add_list_recursion num (x:xs) = x + num : add_list_recursion num xs

add_list_map :: Integer -> [Integer] -> [Integer]
add_list_map num = map (\i -> num + i)

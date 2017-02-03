-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #1
-- Written by Alan Yepez

module Unique where

unique :: Eq a =>[a] -> [a]
unique [] = []
-- unique (x:xs) = x : unique (filter (x /=) xs)
unique list = if checklist list
  then unique (init list)
  else unique (init list) ++ [last list]

checklist :: Eq a => [a] -> Bool
checklist list = elem (last list) (init list)
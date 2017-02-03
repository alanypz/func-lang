-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #1
-- Written by Alan Yepez

module DeleteNthElement where

deleteNthElement :: Int -> [a] -> [a]
deleteNthElement num list = take num list ++ drop (num + 1) list
-- Programming Languages, COP 4020
-- Fall 2015
-- Homework #3
-- Written by Alan Yepez

module Matrix (Matrix, fillWith, fromRule, numRows, numColumns, 
               at, mtranspose, mmap, add, mult) 
where

-- newtype is like "data", but has some efficiency advantages
newtype Matrix a = Mat ((Int,Int),(Int,Int) -> a)

fillWith   :: (Int,Int) -> a -> (Matrix a)
fromRule   :: (Int,Int) -> ((Int,Int) -> a) -> (Matrix a)
numRows    :: (Matrix a) -> Int
numColumns :: (Matrix a) -> Int
at         :: (Matrix a) -> (Int, Int) -> a
mtranspose :: (Matrix a) -> (Matrix a)
mmap       :: (a -> b) -> (Matrix a) -> (Matrix b)
add        :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)
mult       :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)

-- Without changing what is above, implement the above functions.

fillWith pair e = Mat (pair, \_ -> e)
fromRule pair rule = Mat (pair, rule)
numRows (Mat ((row, col), _)) = row
numColumns (Mat ((row, col), _)) = col
at (Mat ((row,col), rule)) (i,j)
  | 1 <= i && i <= row && 1 <= j && j <= col = rule (i,j)
  | otherwise = error "Error: Index out of range"
mtranspose (Mat ((row, col), rule)) = Mat ((col,row), \(i,j) -> rule (j,i))
mmap f (Mat (pair, rule)) = Mat (pair, \e -> f (rule e))
add x y
  | numRows x /= numRows y || numColumns x /= numColumns y = error "Error: Matrices not compatable."
  | otherwise = fromRule (numRows x, numColumns y) (\rule -> (at x rule) + (at y rule))
mult x y
  | numColumns x /= numRows y = error "Error: Matrices not compatable."
  | otherwise = fromRule (numRows x, numColumns y) rule
     where rule (row, col) = sum [ a * b | z <- [1 .. (numColumns x)],  
                                let a = at x (row,z), 
                                let b = at y (z,col)]
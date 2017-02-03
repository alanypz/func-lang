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

at (Mat (_, rule)) = rule

mtranspose (Mat (pair, rule)) = Mat (pair, \(row,col) -> rule (col,row))
-- mtranspose (Mat (pair, rule)) = Mat (pair, rev)
--    where rev (row ,col) = rule (col,row)

mmap f (Mat (pair, rule)) = Mat (pair, f.rule)
-- mmap f (Mat (pair, rule)) = Mat (pair, \e -> f (rule e))

--add (Mat (pair1, rule1)) (Mat (pair2, rule2)) = Mat(pair1, (rule1 + rule2))

arithmetic operation x y
   | numRows x /= numRows y = error "Incompatable matrixes"
   | numColumns x /= numColumns y = error "Incompatable matrixes"
   | otherwise = fromRule (numRows x, numColumns y) (\z -> operation (at x z) (at y z))
      
add x y = arithmetic (+) x y
mult x y = arithmetic (-) x y 

{-
add x y = verify  x (+) y
mult x y = verify x (*) y
verify x f y
   | numRows x /= numRows y || numColumns x /= numColumns y = error "Incompatable matrix sizes."
   | otherwise = fromRule (numRows x, numColumns y) (\rule -> f (at x rule) (at y rule))
-}

{-
add x y = arithmetic (+) x y
mult x y = arithmetic (-) x y 

arithmetic operation x y
   | numRows x /= numRows y = error "Incompatable matrixes"
   | numColumns x /= numColumns y = error "Incompatable matrixes"
   | otherwise = fromRule (numRows x, numColumns y) (\z -> operation (at x z) (at y z))
-}

-----

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
  | 1 <= i || i <= row || 1 <= j || j <= col = rule (i,j)
  | otherwise = error "Error: Index out of range"
mtranspose (Mat (pair, rule)) = Mat (pair, \(row,col) -> rule (col,row))
mmap f (Mat (pair, rule)) = Mat (pair, \e -> f (rule e))
add x y
  | numRows x /= numRows y || numColumns x /= numColumns y = error "Error: Matrices not compatable."
  | otherwise = fromRule (numRows x, numColumns y) (\rule -> (at x rule) + (at y rule))
mult x y
  | numColumns x /= numRows y = error "Error: Matrices not compatable."
  | otherwise = fromRule (numRows x, numColumns y) rule
     where rule (row, col) = sum[ a * b | z <- [1 .. (numColumns x)],  
                                let a = at x (row,z), 
                                let b = at y (z,col)]

{-
Previous attempts/failures at mult.

mult x y
  | numColumns x /= numRows y = error "Error: Matrices not compatable."
  | otherwise = fromRule (m1, n1) rule
     where rule (row, col) = sum[ a * b | i <- [1 .. smlr],  let a = x `at` (row,i), let b = y `at` (i,col)]
           m1 = numRows x
           n1 = numColumns y
           smlr = if m1 > n1 then n1 else m1

mult x y
  | numColumns x /= numRows y = error "Error: Matrices not compatable."
  | otherwise = fromRule (m1, n1)
     (\rule -> sum[a * b | i <- [1 .. m1], j <- [1..n1], let a = x `at` (i,_), let b = y `at` (_,j)])
     where m1 = numRows x
           n1 = numColumns y

  mult x y
  | numColumns x /= numRows y = error "Error: Matrices not compatable."
  | otherwise = fromRule (numRows x, numColumns y) math
  where math = (\rule -> (at x rule) * (at y rule))
-}

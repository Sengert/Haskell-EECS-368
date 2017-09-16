-----------------------------------------------------------------------------------------------------------------------------------------------
--Last Updated 5/3/2016 - 10:09 PM
--
--Samuel Engert & Levi Clark
-- 2613739         2796249
--
--EECS 368
--Haskell Project
--
--
--Collaborated with Matt Kravitz and Alex Pechin 
-----------------------------------------------------------------------------------------------------------------------------------------------

{-
Imports - Data.Char, Data.List, Data.List.Split, Data.Maybe, Data.String
-}
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String


-------------------------------------------------------------------------------------------------------------
------------------------------------------------Lotus Solver (signature function)----------------------------
-------------------------------------------------------------------------------------------------------------


-- @Pass a list of Ints containing 49 elements that we want to solve by replacing the 0's with the correct numbers to solve a Lotus Sudoku Board
-- @Returns Either and empty list if the given list is unsolvable, or a list with no 0's left which represents a solved Lotus Sudoku Board
-- make calls on other functions and indices here
lotusSolver:: [Int] -> [Int]
lotusSolver unsolvedList 
	| thisInd == Nothing = unsolvedList
	| otherwise = checkPossible unsolvedList possiblesList (fromJust thisInd)
   where thisInd = elemIndex 0 unsolvedList
         possiblesList = getPosValues unsolvedList (fromJust thisInd) 1


-------------------------------------------------------------------------------------------------------------
------------------------------------------------Variables----------------------------------------------------
-------------------------------------------------------------------------------------------------------------

-- A list of lists of Ints [[Int]]
-- Indexes the indices of a list of 49 values into 7 rings going from outer-most to inner-most
rings = chunksOf 7 [0..48]::[[Int]]



-- A list of lists of Ints [[Int]]
-- Indexes the indices of a list of 49 values into 7 arms going clockwise
clkWiseSwirl = [[0,7,15,22,30,37,45],
              [1,8,16,23,31,38,46],
              [2,9,17,24,32,39,47],
              [3,10,18,25,33,40,48],
              [4,11,19,26,34,41,42],
              [5,12,20,27,28,35,43],
              [6,13,14,21,29,36,44]]::[[Int]]

-- A list of lists of Ints [[Int]]
-- Indexes the indices of a list of 49 values into 7 arms going counter-clockwise
ctrclkWiseSwirl = [[0,13,20,26,33,39,46],
                 [1,7,14,27,34,40,47],
                 [2,8,15,21,28,41,48],
                 [3,9,16,22,29,35,42],
                 [4,10,17,23,30,36,43],
                 [5,11,18,24,31,37,44],
                 [6,12,19,25,32,38,45]]::[[Int]]



-------------------------------------------------------------------------------------------------------------
------------------------------------------------Getters for Rings/Swirls-------------------------------------
-------------------------------------------------------------------------------------------------------------
					 
-- @Pass in an index x from the unsolved list
-- @Returns which circle contains the given index
-- rings are indexed as the outermost circle is 0, the innermost circle is 6
getRing:: Int -> Int
getRing x
   | elem x [0..6] = 0
   | elem x [7..13] = 1
   | elem x [14..20] = 2
   | elem x [21..27] = 3
   | elem x [28..34] = 4
   | elem x [35..41] = 5 
   | elem x [42..48] = 6
   | otherwise = 0
			
			
-- @Pass in an index x from the unsolved list
-- @Returns which clockwise arm contains the given index
-- clockwise arms are indexed as the top arm(contains 0) is arm 0, the next arm moving clockwise is 1, etc.. to 6
getclkWiseSwirl::Int -> Int
getclkWiseSwirl x
   | elem x [0,7,15,22,30,37,45] = 0
   | elem x [1,8,16,23,31,38,46] = 1
   | elem x [2,9,17,24,32,39,47] = 2
   | elem x [3,10,18,25,33,40,48] = 3
   | elem x [4,11,19,26,34,41,42] = 4
   | elem x [5,12,20,27,28,35,43] = 5
   | elem x [6,13,14,21,29,36,44] = 6
   | otherwise = 0

   
-- @Pass in an index x from the unsolved list
-- @Returns which counter-clockwise arm contains the given index
-- counter-clockwise arms are indexed as the top arm(contains 0) is arm 0, the next arm moving counter-clockwise is 1, etc.. to 6
getCtrclkWiseSwirl::Int -> Int
getCtrclkWiseSwirl x
   | elem x [0,13,20,26,33,39,46] = 0
   | elem x [1,7,14,27,34,40,47] = 1
   | elem x [2,8,15,21,28,41,48] = 2
   | elem x [3,9,16,22,29,35,42] = 3
   | elem x [4,10,17,23,30,36,43] = 4
   | elem x [5,11,18,24,31,37,44] = 5
   | elem x [6,12,19,25,32,38,45] = 6
   | otherwise = 0

-------------------------------------------------------------------------------------------------------------
-----------------------------------Functions Solving the Lotus Sudoku Board----------------------------------
-------------------------------------------------------------------------------------------------------------
  

  
-- @Pass in an Int designating which structure we want to access, and an Int of the index being used in getPosValues
-- @Returns a list of Ints of the specific structure(swirls/circles) that we want to check
indexFinder:: Int -> Int -> [Int]
indexFinder which index
	| which == 1 = (clkWiseSwirl !! (getclkWiseSwirl index))
	| which == 2 = (ctrclkWiseSwirl !! (getCtrclkWiseSwirl index))
	| which == 3 = (rings !! (getRing index))

-- @Pass in the unsolvedList, an int for which structure we want, int for which index to check, and an int for a value to check against
-- @Returns an Int, 1 or 0 representing true or false
getPosValuesHelper:: [Int] -> Int -> Int -> Int -> Int
getPosValuesHelper unsolvedList which index a
	| ((unsolvedList !! ((indexFinder which index) !! 0) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 1) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 2) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 3) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 4) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 5) /= a)
		&&(unsolvedList !! ((indexFinder which index) !! 6) /= a))
		 = 1
	| otherwise = 0
	
	
-- @Pass in the unsolved list, an Int of the index of the unsolved list, and an Int of the value to test
-- @Returns a list of Ints which are possible numbers to populate the given index with
getPosValues:: [Int]-> Int -> Int -> [Int]
getPosValues unsolvedList index a
	|(a < 8) = 
		if (a < 8)
		  &&(((getPosValuesHelper unsolvedList 1 index a) == 1)
		  &&((getPosValuesHelper unsolvedList 2 index a) == 1)
		  &&((getPosValuesHelper unsolvedList 3 index a) == 1))
		  then getPosValues unsolvedList index (a + 1) ++ [a]
		else getPosValues unsolvedList index (a + 1)
	|otherwise = []


-- @Pass in the unsolved list, a list of possible values, and an index of the unsolved list at which it tests the list of possible values at
-- @Returns a solved list
checkPossible:: [Int] -> [Int] -> Int -> [Int]
checkPossible _ [] _ = []
checkPossible unsolvedList (x:xs) n
	| solution == [] = checkPossible unsolvedList xs n
	| otherwise = solution
   where solution = solveMe unsolvedList n x


-- @Pass a new slightly more solved version of the unsolved list, an index of the list, a value to place at that index
-- @Returns a solved list
solveMe:: [Int] -> Int -> Int -> [Int]
solveMe xs a test
	| elemIndex 0 xs == Nothing = xs
	| otherwise = slt
	where slt = lotusSolver (let (ys,_:zs) = splitAt a xs 
								in ys ++ [test] ++ zs) 


main = putStrLn $ show (lotusSolver [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0])								

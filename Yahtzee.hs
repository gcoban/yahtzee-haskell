{-|
	Gökhan Çoban
	mail@gcoban.com
|-}

module Yahtzee where

import Data.Ord
import Data.List
import Data.List.Split
import Data.Array.Unboxed
import Data.Algorithm.Munkres

----------------------------------------------
-- main function handles IO operations and runs the algorithm
main :: IO ()
main = do
	contents <- readFile "input.txt"						-- read input file
	let games = map readGame $ chunksOf 13 $ lines contents -- read each 13 lines of file as a Game
	let	results = map getResult games						-- get results for each game
	resultIter results										-- foreach result
	where
		resultIter :: [[Int]] -> IO ()						-- returns to screen
		resultIter []      = return ()						-- base case returns nothing
		resultIter results = do
			print $ head results							-- print first item
			resultIter $ tail results						-- call itself for other items
----------------------------------------------


----------------------------------------------
-- BEGIN: TYPES & READ FUNCTIONS 
-- reads String as Int
readInt :: String -> Int
readInt = read

type Round = [Int]
-- reads String as Round
readRound :: String -> Round
readRound roundLine = map readInt . words $ roundLine

type Game = [Round]
-- reads String List as Game
readGame :: [String] -> Game
readGame roundLines = map readRound roundLines
-- END: TYPES & READ FUNCTIONS
----------------------------------------------


----------------------------------------------
-- BEGIN: HELPER FUNCTIONS
-- checks if all the items of list are same
allTheSame (x:xs) = all (==x) xs
-- END: HELPER FUNCTIONS
----------------------------------------------


----------------------------------------------
-- BEGIN: POINT CALCULATIONS FOR EACH CATEGORY
-- generic function for currying
-- xs - sum of all xs in thrown
calcXs :: Int -> Round -> Int
calcXs x round = sum [y | y <- round, x == y]

-- ones - sum of all ones thrown
calcOnes = calcXs 1

-- twos - sum of all twos thrown
calcTwos = calcXs 2

-- threes - sum of all threes thrown
calcThrees = calcXs 3

-- fours - sum of all fours thrown
calcFours = calcXs 4

-- fives - sum of all fives thrown
calcFives = calcXs 5

-- sixes - sum of all sixes thrown
calcSixes = calcXs 6

-- chance - sum of all dice
calcChance = sum

-- checks if the round provides at least x dice have same value
-- 1) calculate how many time each die [1..6] found in the round
-- 2) true if any of them found more than x time
isXOfAKind :: Int -> Round -> Bool
isXOfAKind x round = any (>=x) [length $ filter (==y) round | y <- [1..6]]

-- generic function for currying
-- x of a kind - sum of all dice, provided at least x dice have same value
calcXOfAKind :: Int -> Round -> Int
calcXOfAKind x round
	| isXOfAKind x round == True = sum round
	| otherwise                  = 0

-- three of a kind - sum of all dice, provided at least three have same value
calcThreeOfAKind = calcXOfAKind 3

-- four of a kind - sum of all dice, provided at least four have same value
calcFourOfAKind = calcXOfAKind 4

-- five of a kind - 50 points, provided all five dice have same value
calcFiveOfAKind round
	| isXOfAKind 5 round == True = 50
	| otherwise                  = 0

-- checks if the round has a straight of x dice
-- 1) sort the given round
-- 2) find the maximum length of straight in the sorted round
-- 3) check if the maximum is bigger than x 
isXStraight x round = calcXStraightIter 1 (sort round) >= x
	where
		calcXStraightIter acc [_] = acc
		calcXStraightIter acc (r1:r2:rs)
			| r2 - r1 == 1 = calcXStraightIter (acc + 1) (r2:rs)
			| otherwise    = max acc $ calcXStraightIter 1 (r2:rs)
	
-- short straight - 25 points, provided four of the dice form a sequence (that is, 1,2,3,4 or 2,3,4,5 or 3,4,5,6)
calcShortStraight round
	| isXStraight 4 round = 25
	| otherwise           = 0

-- long straight - 35 points, provided all dice form a sequence (1,2,3,4,5 or 2,3,4,5,6)
calcLongStraight round
	| isXStraight 5 round = 35
	| otherwise           = 0

-- full house - 40 points, provided three of the dice are equal and the other two dice are also equal. (for example, 2,2,5,5,5)
calcFullHouse round
	| isFullHouse round = 40
	| otherwise         = 0
	where
		-- checks if the round is full house
		-- 1) sort the given round
		-- 2) check if the sorted round is something like [x,x,y,y,y] or [x,x,x,y,y]
		isFullHouse round = any (\x -> x) [(allTheSame $ take x sortedRound) && (allTheSame $ drop x sortedRound) | x <- [2, 3]]
			where
				sortedRound = sort round
-- END: POINT CALCULATIONS FOR EACH CATEGORY
----------------------------------------------


----------------------------------------------
-- BEGIN: ALGORITHM FUNCTIONS
-- calculates points of round for each category
calcAllCategories :: Round -> [Int]
calcAllCategories r =
	[calcOnes r, calcTwos r, calcThrees r, calcFours r, calcFives r, calcSixes r, calcChance r, calcThreeOfAKind r, calcFourOfAKind r, calcFiveOfAKind r, calcShortStraight r, calcLongStraight r, calcFullHouse r]

-- get calculations of each category for the given game
getCalcs :: Game -> [Int]
getCalcs game = concat $ transpose $ map calcAllCategories game

-- get calculations as (max - calc instead of calc) of each category for the given game
getCalcsForMaxProfit :: Game -> [Int]
getCalcsForMaxProfit game = map (\e -> maxValue - e) calcs
	where
		calcs = getCalcs game
		maxValue = maximum calcs

-- creates matrix from calculations
createMatrix :: [Int] -> UArray (Int,Int) Int
createMatrix calcs = listArray ((1,1),(13,13)) calcs

-- find the best calculations for each category
-- after hungarion method find the best assignments, with indexes hungarion return, find values from matrix
findMaxForEachCategory :: Game -> [Int]
findMaxForEachCategory game = [matrix ! hr | hr <- (sort $ fst hungarianResult)]
	where
		-- matrix of normal calculations
		matrix = createMatrix (getCalcs game)
		
		-- as hungarion implementation of haskell doesn't support finding max cost we need to negate calculations
		-- as hungarion implementation of haskell doesn't support negative calculations, we will use (max - calc) method (getCalcsForMaxProfit)
		hungarianResult :: ([(Int, Int)], Int)
		hungarianResult = hungarianMethodInt (createMatrix (getCalcsForMaxProfit game))
		
-- calculates the bonus of 35 points if the sum of the first six categories is 63 or greater
getBonus :: [Int] -> Int
getBonus result
	| sum (take 6 result) > 63 = 35
	| otherwise				   = 0
		
-- runs algorithm, adds bonus and total to the result
getResult :: Game -> [Int]
getResult game = result ++ [bonus, total]
	where
		result = findMaxForEachCategory game
		bonus = getBonus result
		total = sum result + bonus
-- END: ALGORITHM FUNCTIONS
----------------------------------------------
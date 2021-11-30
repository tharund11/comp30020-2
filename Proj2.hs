{- Written by Tharun Dharmawickrema
   18th October 2021
   This program implements a Battleship-style game, where it plays the roles
   of both the searcher and the hider. -}

{- This file 'Proj2.hs' is the main and only file, and it creates a module
   called Proj2. The functions in this module are for either the searcher or
   the hider.
   
   The hider will hide 3 ships in 3 Locations on the grid. The searcher will
   attempt to correctly guess all 3 Locations to win the game. For each guess,
   the hider will give feedback to the searcher, which the searcher uses to 
   improve their next guess. The code will attempt to find the hidden ships 
   in as few as possible number of guesses.
-}

module Proj2 (Location, toLocation, fromLocation, feedback, GameState, 
              initialGuess, nextGuess) where

import Data.List

-- This user-defined data type represents the columns on the grid. Z and I are 
-- included to make the checking of valid Locations easier.
data Column = Z|A|B|C|D|E|F|G|H|I
    deriving (Show, Eq, Ord, Enum)
    

-- This type synonym is used to represent the rows on the grid. This is used
-- to make the code easier to understand rather than just saying Int.
type Row = Int


-- This user-defined data type represents a location on the grid. Each location
-- has a column and a row.
data Location = Location Column Row
    deriving (Show, Eq, Ord)
    

-- This type synonym is used to represent a GameState as a list of lists of
-- Location. GameState stores a list of remaining possible targets.
type GameState = [[Location]]


-- Function that takes a Location and returns the String version of it.
fromLocation :: Location -> String
fromLocation (Location col row) = show col ++ show row


-- Function that takes a String and if length is correct, calls another
-- function to create the Location. If incorrect length, returns Nothing.
toLocation :: String -> Maybe Location
toLocation inputString
    | length inputString == 2 = checkValidLocation inputString
    | otherwise = Nothing
    

-- Function that takes a 2 character String and returns the Location if valid.
-- If not valid, returns Nothing.
checkValidLocation :: String -> Maybe Location
checkValidLocation (x:xs)
    | validcol && validrow = Just (Location col row)
    | otherwise = Nothing
    where (validcol, col) = checkColumn x
          (validrow, row) = checkRow xs
          

-- Function that checks if given column is valid and converts to Column type.
checkColumn :: Char -> (Bool, Column)
checkColumn x
    | x == 'A' = (True, A)
    | x == 'B' = (True, B)
    | x == 'C' = (True, C)
    | x == 'D' = (True, D)
    | x == 'E' = (True, E)
    | x == 'F' = (True, F)
    | x == 'G' = (True, G)
    | x == 'H' = (True, H)
    | otherwise = (False, I)


-- Function that checks if given row is valid and converts to Row type.
checkRow :: [Char] -> (Bool, Row)
checkRow [x]
    | (1 <= num) && (num <= 4) = (True, num)
    | otherwise = (False, -1)
    where num = read [x]::Row


-- Function that takes a target and a guess and returns the feedback for it
-- based on how close the guess is to the target. 
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target guess = ((length matches), (length oneNMatches), 
                            (length twoNMatches))
    where matches = intersect target guess -- find exact matches
          remainingGuesses = guess \\ matches -- remove matches from guesses
          oneNMatches = get1NeighbourMatches target remainingGuesses
          lastGuesses = remainingGuesses \\ oneNMatches
          twoNMatches = get2NeighbourMatches target lastGuesses


-- Function that will return all the guesses one space away from the targets,
-- by recursively checking all the spaces near each target for matches.
get1NeighbourMatches :: [Location] -> [Location] -> [Location]
get1NeighbourMatches [] _ = []
get1NeighbourMatches (x:xs) guesses = (matches) ++ 
                                       get1NeighbourMatches xs newguesses
                    where matches = intersect (generate1Neighbours x) guesses
                          newguesses = guesses \\ matches


-- Function that iteratively generates all the valid Locations that are exactly
-- one space away from the given Location.
generate1Neighbours :: Location -> [Location]
generate1Neighbours (Location col row) = filter validLocation [(Location j i) | 
                                                  i <- [prevrow, row, nextrow], 
                                                 j <- [prevcol, col, nextcol]]
            where nextcol = succ col 
                  prevcol = pred col -- next and previous rows and columns
                  nextrow = succ row
                  prevrow = pred row


-- Function that checks if given Location is on the grid or not.
validLocation :: Location -> Bool
validLocation (Location col row) = (elem col [A,B,C,D,E,F,G,H]) 
                                    && (elem row [1..4])


-- Function that will return all the guesses two spaces away from the targets,
-- by recursively checking all the Locations two spaces near each target for
-- matches. The two space Neighbours are the one space Neighbours of the one 
-- space Neighbours of each target.
get2NeighbourMatches :: [Location] -> [Location] -> [Location]
get2NeighbourMatches [] _ = []
get2NeighbourMatches (x:xs) guesses = (matches) ++ 
                                       get2NeighbourMatches xs newguesses
     where matches = intersect (
                     nub (generate2Neighbours (generate1Neighbours x))) guesses
           newguesses = guesses \\ matches


-- Function that recursively generates all the valid Locations that are exactly
-- two spaces away. 
generate2Neighbours :: [Location] -> [Location]
generate2Neighbours [] = []
generate2Neighbours (x:xs) = (generate1Neighbours x) ++ generate2Neighbours xs


-- Function that takes no input arguments and returns a pair of the inital
-- guess to start the game and the inital GameState. The inital guess was
-- chosen after trial and error (the diagonal is likely to give more 
-- information than others). The inital GameState is all the possible guesses
-- at the start.
initialGuess :: ([Location], GameState)
initialGuess = (firstGuess, initialTargets)
    where firstGuess = [(Location A 1), (Location D 2), (Location H 4)]
          states = [(Location j i)| i <- [1..4], j <- [A,B,C,D,E,F,G,H]]
          initialTargets = getInitialTargets (subsequences states)


-- Function that takes a list of permutations of all possible guesses and 
-- returns a list of the guesses that are length 3.
getInitialTargets :: [[Location]] -> [[Location]]
getInitialTargets [] = []
getInitialTargets (x:xs)
    | length x == 3 = x : getInitialTargets xs
    | otherwise = getInitialTargets xs


-- Function that takes a pair of the previous guess and GameState as input and
-- returns the next guess and the new GameState. Next guess is the first guess
-- in the updated GameState.
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (prevGuess, prevGameState) scores = (guess, nextGameState)
    where updatedGameStates = updateGameState prevGameState prevGuess scores
          nextGameState = delete prevGuess updatedGameStates --delete prev guess
          guess = nextGameState !! 0 
              

-- Function that takes a GameState, previous guess and corresponding score, and
-- returns an updated GameState. It removes any inconsistent targets in the
-- GameState that doesn't have the exact same score as was received in input.
updateGameState :: [[Location]] -> [Location] -> (Int,Int,Int) -> [[Location]]
updateGameState [] _ _ = []
updateGameState (x:xs) guess score
    | (feedback x guess) == score = x : updateGameState xs guess score
    | otherwise = updateGameState xs guess score

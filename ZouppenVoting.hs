module ZouppenVoting where

import Data.List
import System.Random
import qualified Data.Map as Map

-- Some terminology:
--
-- A voting ticket consists of two votes; one for the best (positive
-- vote) and one for the worst candidate (negative vote) as seen by
-- the voter.
--
-- Commonly used abbreviations are 'r' for random generator and 'c'
-- for candidate type.


-- A single voting ticket. The first argument is a vote for the best
-- candidate and the second one is for the worst.  Map requires a vote
-- to be ordered (Ord) so Equality (Eq) is not enough.
data Ord c => Ticket c = Ticket {
      best  :: c,
      worst :: c
    } deriving Show

data Ord c => Result c = Result {
      votes :: Integer,
      candidate :: c
    } deriving Show

-- Checks if a vote is valid and contains real candidates
checkVote :: Ord c => [c] -> (Ticket c) -> Bool
checkVote candidates ticket
    | best ticket == worst ticket = False
    | otherwise = and [(best ticket) `elem` candidates,
                       (worst ticket) `elem` candidates]

-- Counts a list of votes and returns a result map
countVotes :: Ord c => [c] -> Map.Map c Integer
countVotes votes = foldr insertVote Map.empty votes
    where insertVote vote prev = Map.insertWith (+) vote 1 prev 

-- Counts "best" votes and returns a map of votes with candidate as a key 
countBestVotes :: (Ord c) => [Ticket c] -> Map.Map c Integer
countBestVotes tickets = countVotes $ map best tickets

-- Converts the vote map to result list ordered by vote count, descending.
-- voteMapToResult voteMap = sortBy sndOrdering $ Map.toList voteMap
--    where sndOrdering a b = compare (snd b) (snd a)

-- Convert vote map to an ordered list. List contains tie breaker values, too
voteMapToList :: Ord c => StdGen -> Map.Map c Integer -> [Result c]
voteMapToList r voteMap = map cleanOrder $ reverse $ sort $ reorder r (Map.toList voteMap)
    where cleanOrder (votes,tie,candidate) = Result votes candidate

-- Reorders values and inserts tie breakers to snd for sorting
reorder :: Ord c => StdGen -> [(c,Integer)] -> [(Integer,Integer,c)]
reorder _ [] = []
reorder r ((candidate,votes):xs) = (votes,(fst newRand),candidate) :
                                   reorder (snd newRand) xs
    where newRand = random r

-- Returns ordered result of the first round. StdGen is needed for tie breaks
firstRound :: (Ord c) => StdGen -> [Ticket c] -> [Result c]
firstRound r tickets = voteMapToList r $ countBestVotes tickets

-- Filter out tickets which have positive vote for a candidate in a given list.
-- This is used to collect negative votes in the second round
filterPositive :: Ord c => [Ticket c] -> [c] -> [Ticket c]
filterPositive tickets candidates = filter hasAnyBest tickets
    where hasBest ticket candidate = (best ticket) == candidate
          hasAnyBest ticket = or $ map (hasBest ticket) candidates

--results :: (Ord c) => StdGen -> [Ticket c] -> 
--results tickets 
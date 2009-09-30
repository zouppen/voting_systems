module ZouppenVoting where

import Data.List
import System.Random
import qualified Data.Map as Map

-- Some terminology:
--
-- A voting ticket consists of two votes; one for the best and one for the worst
-- candidate as seen by the voter.


-- A single voting ticket. The first argument is a vote for the best candidate
-- and the second one is for the worst.
-- Map requires a vote to be ordered (Ord) so Equality (Eq) is not enough.
data Ord a => Ticket a = Ticket {
      best  :: a,
      worst :: a
    } deriving Show


-- Checks if a vote is valid and contains real candidates
checkVote :: Ord v => [v] -> (Ticket v) -> Bool
checkVote candidates ticket
    | best ticket == worst ticket = False
    | otherwise = and [(best ticket) `elem` candidates,
                       (worst ticket) `elem` candidates]

-- Counts list of votes and returns a result map
countVotes :: Ord v => [v] -> Map.Map v Integer
countVotes votes = foldr insertVote Map.empty votes
    where insertVote vote prev = Map.insertWith (+) vote 1 prev 

-- Counts "best" votes and returns a map of tuples (candidate,votes) 
countBestVotes :: (Ord v) => [Ticket v] -> Map.Map v Integer
countBestVotes tickets = countVotes $ map best tickets

-- Converts the vote map to result list ordered by vote count, descending.
voteMapToResult voteMap = sortBy sndOrdering $ Map.toList voteMap
    where sndOrdering a b = compare (snd b) (snd a)

-- Convert vote map to an ordered list. List contains tie breaker values, too
voteMaptoList :: Ord v => StdGen -> Map.Map v Integer -> [(Integer,v)]
voteMaptoList r voteMap = map cleanOrder $ reverse $ sort $ reorder r (Map.toList voteMap)
    where cleanOrder (votes,tie,candidate) = (votes,candidate)

-- Reorders values and inserts tie breakers to snd for sorting
reorder :: Ord v => StdGen -> [(v,Integer)] -> [(Integer,Integer,v)]
reorder r ((candidate,votes):xs) = (votes,(fst newRand),candidate) :
                                   reorder (snd newRand) xs
    where newRand = random r
reorder _ [] = []

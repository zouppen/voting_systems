module ZouppenVoting where

import qualified Data.Map as Map

-- Some terminology:
---
-- A ballot paper consists of two votes; one for the best and one for the worst
-- candidate as seen by the voter.



-- A single ballot paper. The first argument is a vote for the best candidate
-- and the second one is for the worst.
-- Map requires a vote to be ordered (Ord) so Equality (Eq) is not enough.
data Ord a => Ballot a = Ballot {
      best  :: a,
      worst :: a
    } deriving Show

-- Trivial vote
-- Takes a list of votes and returns the winner
--countVotes :: Ord t => [Vote t] -> t
--countVotes (x:xs) = best x

-- Checks if a vote is valid and contains real candidates
checkVote :: Ord t => [t] -> (Ballot t) -> Bool
checkVote candidates vote
    | best vote == worst vote = False
    | otherwise = and [(best vote) `elem` candidates,
                       (worst vote) `elem` candidates]


countVotes :: Ord t => [t] -> Map.Map t Integer
countVotes votes = foldr insertVote Map.empty votes
    where insertVote vote prev = Map.insertWith (+) vote 1 prev 

--countBestVotes votes = coun
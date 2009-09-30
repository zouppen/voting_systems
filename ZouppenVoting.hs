module ZouppenVoting where

-- A single vote
data Vote a b = Vote a b deriving Show

best (Vote a _) = a 
worst (Vote _ a) = a 

-- Trivial vote
-- Takes a list of votes and returns the winner
countVotes :: [Vote t t] -> t
countVotes (x:xs) = best x

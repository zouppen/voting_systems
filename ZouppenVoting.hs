module ZouppenVoting where

-- A single vote. The first argument is a ballot for the best candidate and the
-- second one is for the worst.
-- Map requires a vote to be ordered (Ord) so Equality (Eq) is not enough.
data Ord a => Vote a = Vote {
      best  :: a,
      worst :: a
    } deriving Show

-- Trivial vote
-- Takes a list of votes and returns the winner
countVotes :: Ord t => [Vote t] -> t
countVotes (x:xs) = best x

-- Checks if a vote is valid and contains real candidates
checkVote :: Ord t => [t] -> (Vote t) -> Bool
checkVote candidates vote
    | best vote == worst vote = False
    | otherwise = True


          
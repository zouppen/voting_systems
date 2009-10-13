module Borda where

import Data.List
import Data.Ratio
--import System.Random
import qualified Data.Map as Map

-- Some terminology:
--
-- A voting ticket consists of multiple votes; 
-- a score for a vote is set by Borda series function. Some Borda series
-- are [n,n-1,...,1] or [1, 1/2, ..., 1/n] for the first vote, second vote
-- and so on.
--
-- Commonly used abbreviations are 'r' for random generator and 'c'
-- for candidate type.


-- A single voting ticket. The list is preferential list, best
-- candidate first.
data Ord c => Ticket c = Ticket [c] deriving Show

-- A result contains points for a candidate. 
data (Num n,Ord c) => Result n c = Result {
      votes :: n,
      candidate :: c
    } deriving (Eq, Show, Ord)

-- A Borda series used in the parliament elections of Nauru. Also
-- used in many political organizations worldwide. [1,1%2,1%3,..]
nauruBorda :: [Rational]
nauruBorda = nauruBorda' 1

nauruBorda' :: Integer -> [Rational]
nauruBorda' n = ((1 % n) : nauruBorda' (n+1))

-- A Borda series used in the Eurovision Song Contest
eurovisionBorda :: [Integer]
eurovisionBorda = [12,10,8,7,6,5,4,3,2,1]

-- A Borda series used commonly in the Social Democratic Party of
-- Finland. [n, n-1, n-2, ... 1]
sdpBorda :: Integer -> [Integer]
sdpBorda 0 = []
sdpBorda n = (n:sdpBorda (n-1))

--ticketToMap :: (Ord k) => [a] -> Ticket k -> Map.Map k a
ticketToMap series (Ticket votes) = Map.fromList $ zip votes series

sumMaps :: (Ord k, Num a) => [Map.Map k a] -> Map.Map k a
sumMaps = Map.unionsWith (+)

ticketsToVoteMap series tickets = sumMaps $ map (ticketToMap series) tickets

countVotes series tickets = mapToResult $ ticketsToVoteMap series tickets

mapToResult :: (Ord n,Num n,Ord c) => Map.Map c n -> [Result n c]
mapToResult = reverse . sort . (map tupleToResult) . Map.toList

tupleToResult (candidate,votes) = Result votes candidate

-- Group tickets. These are rarely used in real world but are useful
-- when simulating elections or planning tactical voting.

groupTicketToMap series (n,ticket) = ticketToMap (map (*(fromInteger n)) series) ticket 

groupTicketsToVoteMap series tickets = sumMaps $ map (groupTicketToMap series) tickets

countGroupVotes series tickets = mapToResult $ groupTicketsToVoteMap series tickets

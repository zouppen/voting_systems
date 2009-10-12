import CSV
import Borda

readTicketFile path = do
  res <- parseFile path
  case res of Left err -> error $ show err
	      Right xs -> return $ extractVotes xs

extractVotes :: [[String]] -> [Ticket String]
extractVotes raw = map ticketify raw
    where ticketify list = Ticket list

resultToText a = ". " ++ (candidate a) ++ ": " ++ (show $ votes a)

pos :: [Integer]
pos = iterate (+1) 1

strPos = map show pos

countToText :: (Ord a,Num a) => [a] -> [Ticket String] -> String
countToText method tickets = unlines $ zipWith (++) strPos $ map resultToText $ countVotes method tickets

countToScreen method tickets = putStrLn $ countToText method tickets

countFile method file = do tickets <- readTicketFile file
                           putStrLn $ countToText method tickets


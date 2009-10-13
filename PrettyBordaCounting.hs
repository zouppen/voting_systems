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

resultsToText :: (Num n) => [Result n String] -> String
resultsToText result = unlines $ zipWith (++) strPos $ map resultToText result

countToScreen method tickets = putStrLn $ resultsToText $ countVotes method tickets

countFile method file = do tickets <- readTicketFile file
                           putStrLn $ resultsToText $ countVotes method tickets

-- Group tickets

readGroupTicketFile path = do
  res <- parseFile path
  case res of Left err -> error $ show err
	      Right xs -> return $ extractGroupVotes xs

extractGroupVotes :: [[String]] -> [(Integer,Ticket String)]
extractGroupVotes raw = map ticketify raw
    where ticketify (n:list) = (read n,Ticket list)

countGroupFile method file = do tickets <- readGroupTicketFile file
                                putStrLn $ resultsToText $ countGroupVotes method tickets

import CSV
import ZouppenVoting

readTicketFile path = do
  res <- parseFile path
  case res of Left err -> error $ show err
	      Right xs -> return $ extractVotes xs

extractVotes :: [[String]] -> [Ticket String]
extractVotes raw = map ticketify raw
    where ticketify [best,worst] = Ticket best worst
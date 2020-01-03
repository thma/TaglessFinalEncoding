type UserName = String

data DataResult = DataResult String
  deriving (Eq, Show)

--requestData :: Monad m => UserName -> m [DataResult]
requestData :: (Cache m, DataSource m) => UserName -> m [DataResult]
requestData userName = do
 cache  <- getFromCache userName
 result <- case cache of
   Just dataResult -> return dataResult
   Nothing         -> getFromSource userName
 storeCache result
 return result


class Monad m => Cache m where
  getFromCache :: String -> m (Maybe [DataResult])
  storeCache :: [DataResult] -> m ()

class Monad m => DataSource m where
  getFromSource :: String -> m [DataResult] 

instance Cache IO where
  getFromCache _ = return Nothing
  storeCache _ = return ()

instance DataSource IO where
  getFromSource user = return $ [DataResult $ "source: " <> user]


main = do
  user <- requestData "John Toturo"
  print user 


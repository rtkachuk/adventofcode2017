main = do
  putStrLn $ "Key: "
  text <- getLine
  let key = map ( read . (:"")) text :: [Int]
  let newKey = ((tail key) ++ [(head key)])
  let (result,_) = unzip $ filter (\(a,b) -> a == b) $ zip key newKey
  putStrLn $ show $ foldl (+) 0 result

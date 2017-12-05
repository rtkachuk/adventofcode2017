main = do
  putStrLn $ "Key: "
  text <- getLine
  let key = map ( read . (:"")) text :: [Int]
  let step = (length key) `div`  2
  let firstHalf = [0..(step-1)]
  let secondHalf = [(step)..(length key)-1]
  let newKey = map (\x -> key !! x) $ secondHalf ++ firstHalf
  let (result,_) = unzip $ filter (\(a,b) -> a == b) $ zip key newKey
  putStrLn $ show $ foldl (+) 0 result

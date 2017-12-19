check :: String -> [String] -> Int
check line lines = let firstWord = head $ words $ line
                   in length $ filter (\item -> elem firstWord $ tail $ words $ filter (/= ',') item) lines

main = do
  dat <- readFile $ "data.txt"
  let arrayOfItems = lines $ dat
  putStrLn $ head $ words $ fst $ head $ filter (\(a,b) -> b == 0) $ zip arrayOfItems $ map (\line -> check line arrayOfItems) arrayOfItems

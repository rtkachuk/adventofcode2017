check :: [String] -> Bool -> Bool
check [] visited = visited
check (word:phrase) visited = if (word `elem` phrase) then
                                check phrase True
                              else
                                check phrase visited

main = do
  dat <- readFile $ "data.txt"
  let phrases = lines dat
  putStrLn $ show $ length $ filter (==False) $ map (\phrase -> check (words phrase) False) phrases

getDivided list val = filter (\elem -> elem `mod` val == 0) list

findDividableValues list = head $ filter (\elem -> length elem >1) $ map (getDivided list) list

process list =
  let values = findDividableValues list
  in (maximum values) `div` (minimum values)

main = do
  dat <- readFile $ "data.txt"
  let arrayOfStrings = lines $ dat
  let arrayOfIntegers = map(\line -> map read (words line) :: [Int]) arrayOfStrings
  putStrLn $ show $ sum $ map (process) arrayOfIntegers

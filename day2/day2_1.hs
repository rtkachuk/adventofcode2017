main = do
  dat <- readFile $ "data.txt"
  let arrayOfStrings = lines $ dat
  let arrayOfIntegers = map(\line -> map read (words line) :: [Int]) arrayOfStrings
  putStrLn $ show $ sum $ map(\line -> (maximum line) - (minimum line)) arrayOfIntegers

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

processNumbers numbers index step
  | index > (length numbers) - 1 = putStrLn $ "Your list: " ++ (show numbers) ++ "\nDone in " ++ (show step) ++ " steps"
  | otherwise = let elementByIndex = numbers !! index
                    changedNumbersList = replaceNth index (elementByIndex + 1) numbers
                in processNumbers changedNumbersList (elementByIndex+index) $ step+1
    
main = do
  dat <- readFile $ "data.txt"
  let numbers = map (\value -> read value :: Int) $ filter (/=[]) $ lines dat
  putStrLn $ "Length: " ++ (show $ length $ numbers)
  processNumbers numbers 0 0

  

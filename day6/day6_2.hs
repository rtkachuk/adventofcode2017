check :: [Int] -> [[Int]] -> Bool
check list lists = [] /= filter (\item -> item == list) lists

changeElement :: Int -> Int -> [Int] -> [Int]
changeElement elem pos list = (take pos list) ++ [elem] ++ (drop (pos+1) list)

getMaximumIndex :: [Int] -> Int
getMaximumIndex list = head $ filter ((== maximum list) . (list !!)) [0..]

process :: Int -> Int -> [Int] -> [Int]
process 0 pos list = list
process number pos list = if (pos < (length list)) then
                            process (number - 1) (pos + 1) $ changeElement ((list !! pos) + 1) pos list
                          else
                            process number 0 list

findListIndex :: [Int] -> [[Int]] -> Int -> Int
--findListIndex item [] num = 0
findListIndex item lists num
  | item == (head lists) = num
  | otherwise = findListIndex item (tail lists) $ num+1

startProgram lists list True num = num - (findListIndex list lists 0)
startProgram lists list False num = let max = maximum list
                                        maxIndex = getMaximumIndex list
                                        newList = changeElement 0 maxIndex list
                                        processedList = process max (maxIndex + 1) newList
                                    in startProgram (lists ++ [processedList]) processedList (check processedList lists) (num+1)

test = [0, 2, 7, 0]
work = [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6]

main = putStrLn $ show $ startProgram [work] work False (0)

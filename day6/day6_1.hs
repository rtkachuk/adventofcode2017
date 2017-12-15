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

startProgram lists list True num = putStrLn $ show $ num
startProgram lists list False num = do
                                    let max = maximum list
                                        maxIndex = getMaximumIndex list
                                        newList = changeElement 0 maxIndex list
                                        processedList = process max (maxIndex + 1) newList
                                    putStrLn $ show $ list
                                    startProgram (processedList:lists) processedList (check processedList lists) (num+1)

test = [0, 2, 7, 0]
work = [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6]

main = startProgram [[ ]] work False (0)

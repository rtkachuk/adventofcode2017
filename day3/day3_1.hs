selectedNumber = 312051

findNearest :: Int -> Int -> Int -> Int
findNearest curr distance num
  | num - (curr ^ 2) > 0 && num - (curr^2) < distance = findNearest (curr+2) (num-(curr^2)) num
  | otherwise = curr

main = do
  let nearestStep = findNearest 1 selectedNumber selectedNumber
  let difference = (nearestStep `div` 2)
  let axes = map (\val -> nearestStep^2-difference*val) [1,3,5,7]
  putStrLn $ show $ axes
  let nearestAxis = map (\value -> abs (value - selectedNumber)) axes
  let nearestAxisIndex = head $ filter ((== minimum nearestAxis) . (nearestAxis !!)) [0..4]
  putStrLn $ show $ axes !! nearestAxisIndex
  putStrLn $ show $ abs(selectedNumber - (axes !! nearestAxisIndex)) + (nearestStep `div` 2)

  
  

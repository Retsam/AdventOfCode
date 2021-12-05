import Data.List (elemIndices, transpose)

main = do
  input <- getContents
  let nums = lines input
  print (part1 nums)
  print (part2 nums)

part1 :: [[Char]] -> Int
part1 nums =
  let gamma = map mostCommonDigit $ transpose nums
      delta = map invert gamma
   in (binToDec gamma * binToDec delta)

part2 :: [String] -> Int
part2 nums =
  let o2Rating = part2Step 0 oxygenCriteria nums
      co2Rating = part2Step 0 co2Criteria nums
   in (binToDec o2Rating * binToDec co2Rating)

onesCount :: String -> Int
onesCount = length . filter (== '1')

mostCommonDigit :: [Char] -> Char
mostCommonDigit x = if onesCount x > (length x `div` 2) then '1' else '0'

invert :: Char -> Char
invert '0' = '1'
invert '1' = '0'
invert _ = error "I thought I saw a two"

binToDec :: Num a => [Char] -> a
binToDec l = sum $ map (2 ^) $ elemIndices '1' $ reverse l

oxygenCriteria :: Int -> Int -> Char
oxygenCriteria ones zeros = if zeros > ones then '0' else '1'

co2Criteria :: Int -> Int -> Char
co2Criteria ones zeros = if ones >= zeros then '0' else '1'

part2Step :: Int -> (Int -> Int -> Char) -> [String] -> String
part2Step i criteria strs =
  let newStrings = considerBit criteria i strs
   in if length newStrings == 1 then head newStrings else part2Step (i + 1) criteria newStrings

considerBit :: (Int -> Int -> Char) -> Int -> [String] -> [String]
considerBit criteria i list =
  let bits = transpose list !! i
      ones = onesCount bits
      bitToFind = criteria ones (length bits - ones)
   in filter (\l -> l !! i == bitToFind) list

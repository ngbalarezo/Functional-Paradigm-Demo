import Data.List (intercalate, group, sort)

wordCountTotal :: String -> Int
wordCountTotal s = length $ words s

formatTuples :: [(String, Int)] -> [String]
formatTuples a = map (\(w, c) -> "  " ++ w ++ ", " ++ show c) a

wordCountEach :: String -> [String]
wordCountEach s = formatTuples $ map (\w -> (head w, length w)) $ group $ sort $ words s

characterCount :: String -> Int
characterCount s = length s

-- MAIN
main :: IO()
main = do
  putStrLn "Sentence Analyzer:\n"
  putStr "Total Word Count: "
  print (wordCountTotal sentence)
  putStrLn "Word Count per Word: "
  putStr (unlines (wordCountEach sentence))
  putStr "Total Character Count: "
  print (characterCount sentence)

sentence :: String
sentence = "The dog is running from the police. The dog is fast but the police are faster."

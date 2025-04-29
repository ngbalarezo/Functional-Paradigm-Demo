import Data.List (group, sort)

wordCountTotal :: String -> Int
wordCountTotal s = length $ words s

wordCountEach :: String -> [(String, Int)]
wordCountEach s = map (\w -> (head w, length w)) $ group $ sort $ words s

characterCount :: String -> Int
characterCount s = length s

-- MAIN
main :: IO()
main = do
  putStrLn "Sentence Analyzer: "
  putStr "  Total Word Count: "
  print(wordCountTotal "The dog is running from the police. The dog is fast, but the police are faster.")
  putStr "  Word Count per Word: "
  print(wordCountEach "The dog is running from the police. The dog is fast, but the police are faster.")
  putStr "  Total Character Count: "
  print(characterCount "The dog is running from the police. The dog is fast, but the police are faster.")

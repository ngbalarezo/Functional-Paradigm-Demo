import Data.Char(isAlphaNum, toLower)
import Data.List (intercalate, group, sort, sortBy)
import Data.Ord (comparing)

wordCount :: String -> Int
wordCount s = length $ words s

characterCount :: String -> Int
characterCount s = length s

formatTuples :: [(String, Int)] -> [String]
formatTuples a = map (\(w, c) -> "  " ++ w ++ ", " ++ show c) a

wordFrequency :: String -> [String]
wordFrequency s = formatTuples 
                $ sortBy (flip (comparing snd)) 
                $ map (\w -> (head w, length w)) 
                $ group 
                $ sort 
                $ words
                $ map (\c -> if isAlphaNum c then toLower c else ' ') s

-- MAIN
main :: IO()
main = do
  putStrLn "=================="
  putStrLn "Sentence Analyzer:"
  putStrLn "==================\n"
  putStr "Total Word Count: "
  print (wordCount sentence) 
  putStr "Total Character Count: "
  print (characterCount sentence)
  putStrLn "Word Frequency: "
  putStr (unlines (wordFrequency sentence))

sentence :: String
sentence = "The dog is running from the police. The dog is fast but the police are faster."

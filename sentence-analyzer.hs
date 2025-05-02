import Data.Char (isAlphaNum, toLower)
import Data.List (group, intercalate, sort, sortBy)
import Data.Ord (comparing)
import Text.Regex.Posix (mkRegex, splitRegex)

sentenceCount :: String -> Int
sentenceCount s = length $ filter (not . null) $ splitRegex (mkRegex "[.!?]+[\\s]*") s

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
  putStr "Sentence Count: "
  print (sentenceCount sentence)
  putStr "Word Count: "
  print (wordCount sentence) 
  putStr "Character Count: "
  print (characterCount sentence)
  putStrLn "Word Frequency: "
  putStr (unlines (wordFrequency sentence))

sentence :: String
sentence = "The dog is running from the police. The dog is fast but the police are faster."

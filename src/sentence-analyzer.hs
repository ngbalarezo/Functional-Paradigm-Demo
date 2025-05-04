import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Text.Regex (mkRegex, splitRegex)

-- Function counts number of sentences, not counting duplicate punctuation as a sentence
sentenceCount :: String -> Int
sentenceCount s = length 
                $ filter (not . null)
                $ splitRegex (mkRegex "[.!?]+[\\s]*") s

-- Function counts number of words by converting the sentence to an array of words and returning the length
wordCount :: String -> Int
wordCount s = length $ words s

-- Function removes the punctuation from a sentence, used in other functions throughout the program
removePunctuation :: String -> String
removePunctuation s = map (\c -> if isAlphaNum c then toLower c else ' ') s 

-- Function converts word frequency tuples by removing the parentheses, returns an array of formatted tuples
formatTuples :: [(String, Int)] -> [String]
formatTuples a = map (\(w, c) -> "       " ++ w ++ ", " ++ show c) a

-- Functions converts sentence into an array of formatted tuples, each with the word and frequency of the word.
-- The array returned is sorted from most common to least common words.
wordFrequency :: String -> [String]
wordFrequency s = formatTuples 
                $ sortBy (flip (comparing snd)) 
                $ map (\w -> (head w, length w)) 
                $ group 
                $ sort 
                $ words
                $ removePunctuation s

-- Function recursively cuts off the front and back of the string until the middle word or two words are left.
middleWord :: String -> String
middleWord s | length (words s) == 1 = head (words s)
             | length (words s) == 2 = s
             | otherwise = middleWord $ unwords $ tail $ init $ words $ removePunctuation s

-- Functions returns the number of characters in the sentence including whitespace characters
characterCount :: String -> Int
characterCount s = length s

-- main
main :: IO()
main = do
  putStrLn "=========================="
  putStrLn "Haskell Sentence Analyzer:"
  putStrLn "==========================\n"
  putStr "Sentence(s): "
  print (sentence)
  putStrLn ""
  putStr "  > Sentence Count: "
  print (sentenceCount sentence)
  putStr "  > Word Count: "
  print (wordCount sentence)
  putStrLn "  > Word Frequency: "
  putStr (unlines (wordFrequency sentence))
  putStr "  > Middle Word(s): "
  print (middleWord sentence)
  putStr "  > Character Count: "
  print (characterCount sentence)
  putStrLn ""

-- Input string
sentence :: String
sentence = "The dog is running from the police. The dog is fast, but the police are faster! The dog panics and screeches."

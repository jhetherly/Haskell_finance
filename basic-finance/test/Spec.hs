import Lib (countWords)

main :: IO ()
main = do

  -- test 1
  input <- readFile "data/AAPL+GOOG+MSFT_nab.csv"
  print $ passedTestMessage "word count per line count" ([2,2,2] == countWords input)


-- print generic messages for tests
passedTestMessage :: String -> Bool -> String
passedTestMessage message test
  | test      = "Passed Test : " ++ message
  | otherwise = "Failed Test : " ++ message

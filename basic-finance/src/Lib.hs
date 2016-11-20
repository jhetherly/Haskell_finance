module Lib
    ( countWords
    , constructFullUrl
    , constructLocalPathName
    , getURLBody
    , getURLCode
    , printErrorMessage
    , processCSV
    ) where

import Data.List ( isInfixOf
                 , delete
                 )
import Network.HTTP ( simpleHTTP
                    , getRequest
                    , getResponseBody
                    , getResponseCode
                    , ResponseCode
                    )
import Text.CSV ( printCSV
                , CSV
                )
import System.FilePath
import Statistics.Correlation.Kendall ( kendall
                                      )
import qualified Data.Vector as Vec

defaultComapnies = "AAPL+GOOG+MSFT"
defaultFields = "nsabpom6m8"

countWords :: String -> [Int]
countWords input = map (length.words) (lines input)

-- nsabpom6m8 = name, symbol, ask, buy, previous close, open,
--              % change from 200 moving average, % change from 50 moving average
constructFullUrl :: String -> [String] -> String
constructFullUrl base [a,b] = base ++ "?s=" ++ a ++ "&f=" ++ b ++ "&e=.csv"
constructFullUrl base [a]   = base ++ "?s=" ++ a ++ "&f=" ++ defaultFields ++ "&e=.csv"
constructFullUrl base _     = base ++ "?s=" ++ defaultComapnies ++ "&f=" ++ defaultFields ++ "&e=.csv"

constructLocalPathName :: String -> [String] -> String
constructLocalPathName dir [a,b] = dir </> a ++ "_" ++ b <.> "csv"
constructLocalPathName dir [a]   = dir </> a ++ "_" ++ defaultFields <.> "csv"
constructLocalPathName dir _     = dir </> defaultComapnies ++ "_" ++ defaultFields <.> "csv"

getURLBody :: String -> IO String
getURLBody url = simpleHTTP (getRequest url) >>= getResponseBody

-- ResponseCode is (Int, Int, Int)
getURLCode :: String -> IO ResponseCode
getURLCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

printErrorMessage :: (Show a) => a -> IO ()
printErrorMessage err = print $ show err

processCSV :: Int -> Int -> CSV -> IO ()
processCSV col1 col2 csv = do
  -- the filter here removes empty lines from the csv file
  let filteredCSV = filter (\e -> not ((length e) == 1 && (e!!0) == "")) csv
  let vec1 = getVectorFromCSV col1 filteredCSV
  let vec2 = getVectorFromCSV col2 filteredCSV
  let kendallTau = kendall $ Vec.zip vec1 vec2
  print $ "The Kendall tau for the selected columns are " ++ (show kendallTau)

-- the first parameter is the csv column to extract
getVectorFromCSV :: Int -> CSV -> Vec.Vector Double
-- getVectorFromCSV :: Int -> CSV -> [String]
getVectorFromCSV col csv =
  Vec.fromList (map (\l -> (read $ val (l!!col))::Double) csv) :: Vec.Vector Double
  -- map (\l -> val (l!!col)) csv
  where
    -- removes all extraneous charaters from number
    val = \l -> [ c | c <- l, not (c `elem` "+%,?!-:;\"\'") ]

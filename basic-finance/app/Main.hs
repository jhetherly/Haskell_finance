module Main where

-- import Control.Exception (catch,
--                           SomeException)
import System.Environment (getArgs)
import Text.CSV ( parseCSVTest
                , parseCSV
                )

import Lib ( countWords
           , constructFullUrl
           , constructLocalPathName
           , getURLBody
           , getURLCode
           , printErrorMessage
           , processCSV
            )

main :: IO ()
main = do

  -- get command line arguments (companies and fields)
  -- ex: stack exec basic-finance-exe AAPL+GOOG+MSFT nab
  -- syntax from this website: http://www.jarloo.com/yahoo_finance/
  -- the '<-' unpacks the [String] from the IO String (pure data from an impure source)
  args <- getArgs

  let urlBase = "http://download.finance.yahoo.com/d/quotes.csv"
  let fullUrl = constructFullUrl urlBase args
  let fullFilePath = constructLocalPathName "data" args

  -- the '<-' unpacks the Int Tuple from the IO ResponseCode (pure data from an impure source)
  resCode <- getURLCode fullUrl
  -- should combine the above and below lines
  let (x,y,z) = resCode
  print $ "Response codes for GET request " ++ fullUrl ++ ": " ++ (show x) ++ "." ++ (show y) ++ "." ++ (show z)

  -- the '<-' unpacks the String from the IO String (pure data from an impure source)
  contents <- getURLBody fullUrl

  let csv = parseCSV fullFilePath contents
  either printErrorMessage (processCSV 6 7) csv

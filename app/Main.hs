module Main where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
-------------------------------------------------------------------------------
main :: IO ()
main = do
    file <- B.readFile "test.tap" 
    B.putStr file
    let result=parse theParser file
    print $ show result
    return ()


-------------------------------------------------------------------------------
theParser::Parser B.ByteString
theParser = do
    str <- many1 line
    endOfInput
    return B.empty

-------------------------------------------------------------------------------
line::Parser B.ByteString
line = do
    chars <- many1 anyWord8 
    endOfLine
    return $ B.pack chars
-------------------------------------------------------------------------------

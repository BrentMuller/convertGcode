module Main where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B

-------------------------------------------------------------------------------
main :: IO ()
main = do
    file <- B.readFile "test.tap" 
    B.putStr file
    let result=parse theParser file
    print $ show result
    return ()


-------------------------------------------------------------------------------
theParser::Parser ()
theParser = do
    return ()

-------------------------------------------------------------------------------
line::Parser ()
line = do
    
    return ()
-------------------------------------------------------------------------------

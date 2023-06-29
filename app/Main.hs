module Main where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
-------------------------------------------------------------------------------
main :: IO ()
main = do
    file <- B.readFile "test.tap" 
    B.putStr file
    doit theParser file
--    let result = parse theParser file
 --   print $ "result: " ++ show result
-------------------------------------------------------------------------------
--parse :: Parser a -> ByteString -> Result a
-- Partial (i -> IResult i r)	
doit::(Show a)=>Parser a -> B.ByteString -> IO () 
doit prsr input = do
    let result = parse prsr input
    case result of
        Fail unconsumed contexts message -> do
            print "Failure:"
            print $ "unconsumed: " ++ show unconsumed 
            print $ "contexts: " ++ show contexts
            print $ "message: " ++ show message
            return ()
        Partial p -> do
            let i = p B.empty -- ::IResult B.ByteString B.ByteString
            print $ "partial ; "  ++ (show i)
            return ()
        Done _ _-> do
            print "Success!"
    print $ "result: " ++ show result
    return ()

-------------------------------------------------------------------------------
--theParser::Parser B.ByteString
theParser::Parser [B.ByteString]
theParser = do
--    str<- manyTill anyWord8 endOfInput
--    str<- manyTill line endOfInput
--    manyTill anyWord8 endOfInput
    str <- many1 line
  --  endOfInput
--    return $ B.concat str
    takeByteString
    return str
--    return $ B.pack str

-------------------------------------------------------------------------------
line::Parser B.ByteString
line = do
    --chars <- many1 anyWord8 
    chars <- manyTill anyWord8 endOfLine
--    endOfLine
    return $ B.pack chars
-------------------------------------------------------------------------------

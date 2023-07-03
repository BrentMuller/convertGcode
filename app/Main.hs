module Main where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 as Atto8
import Data.ByteString.Char8 as BC
-------------------------------------------------------------------------------
main :: IO ()
main = do
    file <- B.readFile "test.tap" 
    B.putStr file
    doit theParser file

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
theParser::Parser [B.ByteString]
theParser = do
    string $ BC.singleton '%'
    endOfLine
    str <- many1 line
    string $ BC.singleton '%'
    endOfLine
    return str
-------------------------------------------------------------------------------
line::Parser B.ByteString
line = do
    words <- choice [comment,manyTill gWord endOfLine ]
    return $ B.concat words
-------------------------------------------------------------------------------
gWord::Parser B.ByteString
gWord = do
   a <-letter_ascii 
   --b <- choice [integerWord,floatWord]
--   b <- choice [integerWord,floatWord,endOfLineByteStr]
   b <-wordRemainder
--   b <- Atto8.takeWhile (Atto8.inClass "-0123456789+.")
   Atto8.takeWhile (\x->x==' ')
   return $ singleton a  `B.append` (BC.pack.show) b `B.append` BC.pack "test"
-------------------------------------------------------------------------------
comment::Parser [B.ByteString]
comment = do
    a<-string $ BC.singleton '(' 
    b<-manyTill anyChar  $ string $ BC.singleton ')' 
    endOfLine
    return $ [BC.pack "comment...." ] ++ [BC.pack b]
-------------------------------------------------------------------------------
wordRemainder::Parser B.ByteString
wordRemainder = do
    a <- Atto8.takeWhile (Atto8.inClass "-0123456789+.")
    return a
-------------------------------------------------------------------------------
-- {-
integerWord::Parser B.ByteString
integerWord = do
   b <-decimal::Parser Int
   return $ (BC.pack.show) b
-------------------------------------------------------------------------------
floatWord::Parser B.ByteString
floatWord = do
    c <-double
    return $ BC.pack $ show c
-- -}
-------------------------------------------------------------------------------
endOfLineByteStr::Parser ByteString
endOfLineByteStr = return empty
-------------------------------------------------------------------------------

module Main where

import Data.Attoparsec.Text.Lazy  as AttoT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import System.FilePath.Posix (dropExtension)
import System.Environment
import System.Exit
import GHC.List
-------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    fileName <- case uncons args of
            Just (a,_) -> return a
            Nothing -> do
               print "no arguments given- exiting.."
               exitWith $ ExitFailure 1
    doit fileName theParser 

-------------------------------------------------------------------------------
doit::FilePath -> Parser [TL.Text] ->  IO () 
doit filePath prsr = do
    input <- TIO.readFile filePath
    let result = parse prsr input
    case result of
        Fail unconsumed contexts message -> do
            print "Failure:"
            print $ "unconsumed: " ++ show unconsumed 
            print $ "contexts: " ++ show contexts
            print $ "message: " ++ show message
            return ()
        Done t r-> do
            TIO.writeFile ((dropExtension filePath) ++ ".out.tap") $ TL.concat r
            print "Success!"
    return ()
-------------------------------------------------------------------------------
theParser::Parser [TL.Text]
theParser = do
    string $ T.singleton '%'
    endOfLine
    str <- many1 line
    string $ T.singleton '%'
    endOfLine
    return $ [TL.pack "%\n"] ++  str ++ [TL.pack "%"]
-------------------------------------------------------------------------------
line::Parser TL.Text
line = do
    words <- choice [comment,manyTill gWord endOfLine ]
    return $ TL.concat words `TL.append` TL.pack "\n"
-------------------------------------------------------------------------------
gWord::Parser TL.Text
gWord = do
   a <-letter
   b <-choice [parseNoLeadingZero,parseNoLeadingZeroMinus,wordRemainder]
   AttoT.takeWhile (\x->x==' ')
   return $ TL.singleton a  `TL.append`  (TL.pack.T.unpack) b `TL.append` TL.pack " "
-------------------------------------------------------------------------------
comment::Parser [TL.Text]
comment = do
    a<-string $ T.singleton '(' 
    b<-manyTill anyChar  $ string $ T.singleton ')' 
    endOfLine
    return $ [TL.pack "(" ] ++ [TL.pack b] ++ [TL.pack ") " ] 
-------------------------------------------------------------------------------
wordRemainder::Parser T.Text
wordRemainder = do
    a <- AttoT.takeWhile (inClass "-0123456789+.")
    return a
-------------------------------------------------------------------------------
parseNoLeadingZero::Parser T.Text
parseNoLeadingZero = do
    a<-char '.' 
    b<-wordRemainder
    return  $ T.pack "0." `T.append` b
-------------------------------------------------------------------------------
parseNoLeadingZeroMinus::Parser T.Text
parseNoLeadingZeroMinus = do
    a<-char '-' 
    b<-char '.'
    c<-wordRemainder
    return  $ T.pack "-0." `T.append`  c
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Program
    (
    program
    ) where

import Data.Char (toUpper)
import System.Exit (exitSuccess)
import qualified Data.ByteString.Char8 as C

import Library (getDict, answer, validateWord)

getInputs :: IO (String, String)
getInputs = (,)
            <$> ( putStrLn "Enter start word of length 4"  >> getLine >>= return . map toUpper)
            <*> ( putStrLn "Enter target word of length 4" >> getLine >>= return . map toUpper)

program :: IO ()
program = do
  (s,t) <- getInputs
  dict  <- getDict

  let wordsAreOk = and . map (validateWord dict)

  if (not $ wordsAreOk [s,t])
    then putStrLn "Bad Input. Try again" >> program
    else do
      putStrLn "Looking for a word chain ..."
      print $ answer dict (C.pack s) (C.pack t)
      putStrLn "____________________________"
      reply <- putStrLn "Try again?(y/n)" >> getLine
      case reply of
        ('y':_) -> program
        ('n':_) -> putStrLn "Bye!" >> exitSuccess
        _       -> putStrLn "I will take it as a no. See you around!" >> exitSuccess

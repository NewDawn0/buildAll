module Colour
  ( Colour (..),
    colour,
    colourify,
    fixLine,
    tmpLine,
  )
where

import System.Console.ANSI (clearLine, restoreCursor, saveCursor, setCursorColumn)
import System.IO (hFlush, stdout)

data Colour = Red | Green | Yellow | Reset
  deriving (Show, Eq)

colour :: Colour -> String
colour Red = "\x1b[1;31m"
colour Green = "\x1b[1;32m"
colour Yellow = "\x1b[1;33m"
colour Reset = "\x1b[0m"

colourify :: Colour -> String -> String
colourify col str = colour col ++ str ++ colour Reset

tmpLine :: String -> IO ()
tmpLine str = do
  saveCursor
  putStr str
  hFlush stdout

fixLine :: String -> IO ()
fixLine str = do
  restoreCursor
  setCursorColumn 0
  clearLine
  putStrLn str

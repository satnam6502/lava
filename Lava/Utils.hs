{-# OPTIONS -fglasgow-exts #-}

-------------------------------------------------------------------------------
--  $Id: Utils.hs#2 2010/10/07 16:17:27 REDMOND\\satnams $
------------------------------------------------------------------------------

-- | The 'Utils' module contains some general utility functions.

module Lava.Utils
where
import System.IO
import Data.List
import Data.Char
import System.Time
import Debug.Trace
import Control.Exception
import System.Directory

--------------------------------------------------------------------------------
-- The Utils module provides miscellenous useful utiltiy functions.
--------------------------------------------------------------------------------

{-| The 'insertString' function inserts one string between every string
    in a list and returns the concatenated result.
    For example insertString "," ["a", "b", "c"] = "a, b, c"
-}

insertString :: String     -- ^ The string to be inserted between strings.
               -> [String] -- ^ The list of strings to be joined by above string
               -> String   -- ^ The result of the insertion
insertString _ [] = []
insertString s str = foldl1 (insertString' s) str
insertString' s x y = x ++ s ++ y

--------------------------------------------------------------------------------

-- The insertCommas function takes a list of strings and returns
-- a string with the input strings concatenated and separated by commas.

insertCommas :: [String] -> String
insertCommas = insertString ", "

--------------------------------------------------------------------------------

-- The insertDotes function takes a list of strings and returns
-- a string with the input strings concatenated and separated by full stops.

insertDots :: [String] -> String
insertDots = insertString "."

--------------------------------------------------------------------------------

makeUpperCase :: String -> String
makeUpperCase = map toUpper

--------------------------------------------------------------------------------

makeLowerCase :: String -> String
makeLowerCase = map toLower

--------------------------------------------------------------------------------

stripSuffix :: String -> String -> String
stripSuffix suffix s 
  = if (take l (reverse s)) == reverse suffix then
      take (length s - l) s 
    else
      s
    where
    l = length suffix

--------------------------------------------------------------------------------

dirPrefix :: String -> String
dirPrefix dirSpec
  = if '/' `elem` dirSpec then
      reverse (dropWhile ((/=)'/') (reverse dirSpec))
    else
      ""

--------------------------------------------------------------------------------

filenameRoot :: String -> String
filenameRoot dirSpec
  = if '/' `elem` dirSpec then
      reverse (takeWhile ((/=)'/') (reverse dirSpec))
    else
      dirSpec


-------------------------------------------------------------------------------

indent_line n line = replicate n ' ' ++ line
indent n  = map (indent_line n) 

-------------------------------------------------------------------------------

insert_semicolons = insert_in_lines " ;"
insert_commas = insert_in_lines " ,"

-------------------------------------------------------------------------------


insert_in_lines str [] = []
insert_in_lines str [a] = [a]
insert_in_lines str (a:b:rest) = (a++str) : insert_in_lines str (b:rest)

-------------------------------------------------------------------------------

pad_with v n l
  = l ++ replicate (n - length l) v

-------------------------------------------------------------------------------

notrace :: String -> a -> a
notrace _ x = x

-------------------------------------------------------------------------------

checkTake :: Show a => String -> Int -> [a] -> [a]
checkTake str n l
  = if n < 0 then
      error ("checkTake: " ++ str ++ ": " ++ show n ++ " of " ++ show l)
    else
      take n l
      
-------------------------------------------------------------------------------

log2 :: Int -> Int
log2 n = log2' n 0

log2' n p 
  = if 2^p >= n then
      p
    else
      log2' n (p+1)
      
-------------------------------------------------------------------------------

bitsrequired :: Integral num => num -> Int
bitsrequired n
  = if n < 0 then
      1 + bitsrequired (abs n)
    else
      countbits n

countbits n
  = if n `div` 2 == 0 then
      1
    else
      1 + countbits (n `div` 2)
      
-------------------------------------------------------------------------------

assert :: Bool -> String -> IO ()
assert check message
  = if check then
      return ()
    else
      error (message ++ "\n")
 
-------------------------------------------------------------------------------

writeDate :: IO ()
writeDate
  = do str <- dateString
       flushPut str

--------------------------------------------------------------------------------

writeDateLn :: IO ()
writeDateLn
  = do str <- dateString
       flushPutLn str

--------------------------------------------------------------------------------

dateString :: IO String
dateString
  = do clockT <- getClockTime
       calTime <- toCalendarTime clockT
       return (calendarTimeToString calTime)

--------------------------------------------------------------------------------

fileExists :: String -> IO Bool
fileExists = doesFileExist 

--------------------------------------------------------------------------------

flushPut :: String -> IO  ()
flushPut s
  = do putStr s
       hFlush stdout

--------------------------------------------------------------------------------

flushPutLn :: String -> IO  ()
flushPutLn s
  = do putStrLn s
       hFlush stdout

--------------------------------------------------------------------------------

singleton :: a -> [a]
singleton a = [a]

--------------------------------------------------------------------------------

{-# INLINE debugPut #-}

debugPut :: Bool -> String -> IO ()
debugPut cond str
  = if cond then
      flushPut str
    else
      return ()

--------------------------------------------------------------------------------

{-# INLINE debugPutLn #-}

debugPutLn :: Bool -> String -> IO ()
debugPutLn cond str
  = if cond then
      flushPutLn str
    else
    
  return ()
--------------------------------------------------------------------------------

{-# INLINE condPutWithDate #-}

condPutWithDate :: Bool -> String -> IO ()
condPutWithDate cond str
  = if cond then
      do date <- dateString
         flushPut (date ++ ": " ++ str)
    else
      return ()

--------------------------------------------------------------------------------

{-# INLINE condPutWithDateLn #-}

condPutWithDateLn :: Bool -> String -> IO ()
condPutWithDateLn cond str
  = if cond then
      do date <- dateString
         flushPutLn (date ++ ": " ++ str)
    else
      return ()

--------------------------------------------------------------------------------

selectElement :: Int -> [a] -> a
selectElement i a = a!!i

--------------------------------------------------------------------------------

unsingleton :: [a] -> a
unsingleton [a] = a

--------------------------------------------------------------------------------

findContainingPower :: (Num num, Ord num) => num -> Int
findContainingPower n = findContainingPower' n 0

--------------------------------------------------------------------------------

findContainingPower' :: (Num num, Ord num) => num -> Int -> Int
findContainingPower' n p
  = if n <= 2^p then
      p
    else
      findContainingPower' n (p+1)

--------------------------------------------------------------------------------

checkListLength :: Int -> String -> [a] -> [a]
checkListLength len msg lst
  = if length lst /= len then
      error ("checkListLength: expected length " ++ show len ++ 
             " but actual length " ++ show (length lst) ++ " : " ++ msg)
    else
      lst

--------------------------------------------------------------------------------

unsafePutStrLn txt a = trace txt a

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Chops a list into chunks

chopList :: Int -> [a] -> [[a]]
chopList n [] = []
chopList n l | length l < n = [l]
chopList n l = take n l : chopList n (drop n l)

-------------------------------------------------------------------------------
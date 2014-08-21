-------------------------------------------------------------------------------
--- $Id: ReportPAR.hs#3 2010/10/01 12:25:40 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where

import Control.Monad
import System.Directory

-------------------------------------------------------------------------------

main :: IO ()
main
  = do cd <- getCurrentDirectory
       ls <- getDirectoryContents cd
       dirs <- filterDirectories ls
       putStrLn "test\tdesign\tpart\tpackage\tspeed\tmet\tfreq\tsetup worst slack\tsetup best\tsetup timing errors\tsetup timing score\thold worst slack\tminperiod worst slack\tminperiod best\tminperiod timing errors\tminperiod timing score"
       mapM_ processDirectory dirs

-------------------------------------------------------------------------------

filterDirectories :: [FilePath] -> IO [FilePath]
filterDirectories [] = return []
filterDirectories (f:fs)
  = do exists <- doesDirectoryExist  f
       ds <- filterDirectories fs
       if exists then
         return (f:ds)
        else
         return ds

-------------------------------------------------------------------------------

isParFileName :: String -> Bool
isParFileName fileName | length fileName >= 4
  = take 4 (reverse fileName) == "rap."
isParFileName other = False

-------------------------------------------------------------------------------

processDirectory :: FilePath -> IO ()
processDirectory dirName
  = do dirls <- getDirectoryContents dirName
       let maybePF = maybeParFile dirls
           Just parFile = maybePF
       when (maybePF /= Nothing) $
          processPARFile dirName parFile

-------------------------------------------------------------------------------

maybeParFile :: [FilePath] -> Maybe FilePath
maybeParFile [] = Nothing
maybeParFile (f:fs)
  = if isParFileName f then
      Just f
    else
      maybeParFile fs

-------------------------------------------------------------------------------

processPARFile :: FilePath -> FilePath -> IO ()
processPARFile dir parFile
  = do parFileContents <- readFile (dir ++ "/" ++ parFile)
       let parFileLines = lines parFileContents
       putStr (dir ++ "\t" ++ (stripSuffix parFile) ++ "\t")
       reportJob parFileLines
       let maybeResults = findPARresults parFileLines
           Just results = maybeResults
           [bars1, contraint1, constraint2, bars2, timespec, hold, minperiod, bars3] = results
           (setupSlack, setupBest, setupErrs, setupScore)
              = getSetupResults timespec
           (holdSlack, holdErrs, holdScore) = getHoldResults hold
       when (maybeResults /= Nothing) $
         do if head timespec == '*' then
              putStr "FAILED\t"
             else
              putStr "PASSED\t" 
            putStr (getTargetFrequency timespec ++ "\t" ++ (deNS setupSlack) ++ 
                    "\t" ++ (deNS setupBest) ++ "\t" ++ setupErrs ++ "\t" ++
                    setupScore ++ "\t" ++ (deNS holdSlack) ++ "\t" ++ holdErrs ++
                    "\t" ++ holdScore)
            putStr (getMinPeriodResults minperiod)
       putStrLn ""

-------------------------------------------------------------------------------

deNS :: String -> String
deNS "" = ""
deNS x = (init . init) x

-------------------------------------------------------------------------------

getTargetFrequency :: String -> String
getTargetFrequency line
  = if stringStartsWith line "  Autotimespec" then
      "AUTO\t"
    else
      freq
    where
    ts:eq:per:grp:clk:freq:_ = words (tail line)    

-------------------------------------------------------------------------------

getSetupResults :: String -> (String, String, String, String)
getSetupResults line
   = (init slack, init best, init timerrs, init score)
     where
     [setup,_,slack,best,timerrs,score] = words (tail (dropWhile ((/=)'|') line))

-------------------------------------------------------------------------------

getHoldResults :: String -> (String, String, String)
getHoldResults line
   = (init slack, init timerrs, init score)
     where
     [hold,_,slack,best,timerrs,score] = words (tail (dropWhile ((/=)'|') line))

-------------------------------------------------------------------------------

getMinPeriodResults :: String -> String
getMinPeriodResults ('-':_) = ""
getMinPeriodResults line
  = init slack ++ "\t" ++  init best ++ "\t" ++ init timerrs ++ "\t" ++ init score
    where
    [setup,_,slack,best,timerrs,score] = words (tail (dropWhile ((/=)'|') line))

-------------------------------------------------------------------------------


findPARresults :: [String] -> Maybe [String]
findPARresults [] = Nothing
findPARresults (l1:l2:l3:l4:l5:l6:l7:l8:rest) | length l1 >= 10 && length l2 >= 12
  = if take 10 l1 == "----------" && take 12 l2 == "  Constraint" then
      if moreResults /= Nothing then
        moreResults
      else
        Just [l1,l2,l3,l4,l5,l6,l7,l8]
    else
      findPARresults (l2:l3:l4:l5:l6:l7:l8:rest)
    where
    moreResults = findPARresults rest
findPARresults (x:xs) = findPARresults xs

-------------------------------------------------------------------------------

stringStartsWith :: String -> String -> Bool
stringStartsWith s prefix
  = length s >= length prefix && take (length prefix) s == prefix

-------------------------------------------------------------------------------

findLineStaringWith :: String -> [String] -> String
findLineStaringWith text [] 
  = error ("Failed to fine line starting with " ++ text)
findLineStaringWith text (line:lines)
  = if stringStartsWith line text then
      line
    else
      findLineStaringWith text lines

-------------------------------------------------------------------------------

findLineAfterStaringWith :: String -> [String] -> String
findLineAfterStaringWith text [] 
  = error ("Failed to fine line starting with " ++ text)
findLineAfterStaringWith text (line:lines)
  = if length line >= length text then
      if take (length text) line == text then
        head lines
      else
        findLineAfterStaringWith text lines
    else
      findLineAfterStaringWith text lines

-------------------------------------------------------------------------------


stripSuffix :: String -> String
stripSuffix 
  = reverse . tail . dropWhile ((/=) '.') . reverse

-------------------------------------------------------------------------------

reportJob :: [String] -> IO ()
reportJob parFileLines
  = putStr (part ++ "\t" ++ package ++ "\t" ++ speed ++ "\t")
    where
    partLine = findNCDLine parFileLines
    partWords = words partLine
    part = extractPart partWords
    package = extractPackage partWords
    speed = extractSpeedGrade partWords

-------------------------------------------------------------------------------

extractPart :: [String] -> String
extractPart [] = error "Failed to extract part number"
extractPart ("device":device:_) = init device
extractPart (w:ws) = extractPart ws

-------------------------------------------------------------------------------

extractPackage :: [String] -> String
extractPackage [] = error "Failed to extract package number"
extractPackage ("package":package:_) = init package
extractPackage (w:ws) = extractPackage ws

-------------------------------------------------------------------------------

extractSpeedGrade :: [String] -> String
extractSpeedGrade [] = error "Failed to extract speed grade"
extractSpeedGrade ("speed":speed:_) = tail speed
extractSpeedGrade (w:ws) = extractSpeedGrade ws

-------------------------------------------------------------------------------

findNCDLine :: [String] -> String
findNCDLine [] = error "Failed to find NCD line"
findNCDLine (l:ls)
  = if length w >= 4 && w1 == "is" && w2 == "an" && w3 == "NCD," then
      l
    else
      findNCDLine ls 
    where
    w = words l
    _:w1:w2:w3:_ = w

-------------------------------------------------------------------------------


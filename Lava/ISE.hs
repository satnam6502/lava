-------------------------------------------------------------------------------
--- $Id: ISE.hs#11 2010/10/04 00:29:53 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.ISE (Effort(..), xflow, implement)
where
import System.Directory
import System.Cmd
import Control.Monad

-- * Circuit implementation with the Xilinx ISE tools


data Effort = Balanced | FastRuntime | HighEffort
              deriving (Eq, Show, Read)

-------------------------------------------------------------------------------

showEffort :: Effort -> String
showEffort effort
  = case effort of
      Balanced -> "balanced"
      FastRuntime -> "fast_runtime"
      HighEffort -> "high_effort"

-------------------------------------------------------------------------------

effortFile :: Effort -> String
effortFile effort
  = case effort of
      Balanced -> "balanaced.opt"
      FastRuntime -> "fast_runtime.opt"
      HighEffort -> "high_effort.opt"

-------------------------------------------------------------------------------
-- | The 'xflow' function executes the Xilinx ISE implementation tools
--   with the specified circuit, part, package, speed grade and
--   implementation effort. The name of the primary clock is assumed
--   to be clk.

xflow :: String    -- ^ The name of the circuit to be implemented
         -> String -- ^ The part to be used for the implementation
         -> String -- ^ The package to be used for the implementation
         -> Int    -- ^ The speed grade to be used for the implementation
         -> Effort -- ^ The implementaiton effort
         -> Int    -- ^ The target operating frequency
         -> IO ()
xflow circuitName part package speedGrade effort frequency
  = do writeFile (circuit ++ ".ucf") 
          (unlines ["NET \"clk\" TNM_NET = clk;",
                    "TIMESPEC TS_clk = PERIOD \"clk\" " ++ show frequency ++
                    " MHz HIGH 50%;"])
       putStrLn command
       system command
       return ()
    where
    circuit = circuitName ++ "_" ++ part ++ "_" ++ show speedGrade ++ 
              "_" ++ package ++ "_" ++ show frequency ++ "MHz_" ++ 
              showEffort effort
    partName = part ++ "-" ++ show speedGrade ++ package
    command = "xflow -wd " ++ circuit ++ " -p " ++ partName ++
               " -synth xst_vhdl.opt " ++ 
               circuitName ++ " -implement " ++ effortFile effort

-------------------------------------------------------------------------------

implement :: String    -- ^ The name of the circuit to be implemented
         -> String -- ^ The part to be used for the implementation
         -> String -- ^ The package to be used for the implementation
         -> Int    -- ^ The speed grade to be used for the implementation
         -> Effort -- ^ The implementaiton effort
         -> Int    -- ^ The target operating frequency
         -> IO ()
implement circuitName part package speedGrade effort frequency
  = do putStrLn ("Implementing " ++ circuit)
       exists <- doesDirectoryExist circuit
       when (not exists) $
         createDirectory circuit
       setCurrentDirectory circuit
       writeFile (circuitName ++ ".prj") 
        ("vhdl work ../" ++ circuitName ++ ".vhd")
       writeFile (circuitName ++ ".ucf") 
          (unlines ["NET \"clk\" TNM_NET = clk;",
                    "TIMESPEC TS_clk = PERIOD \"clk\" " ++ show frequency ++
                    " MHz HIGH 50%;"])
       writeFile (circuitName ++ ".xst")
          (unlines (xstOptions circuitName partName))
       system ("xst -intstyle silent -ifn " ++ circuitName ++ ".xst -ofn " ++
                circuitName ++ ".syr")
       system ("ngdbuild -intstyle silent -quiet -sd .. -dd _ngo -nt timestamp -uc " ++ circuitName ++ ".ucf -p " ++ partName ++ " " ++ circuitName ++ ".ngc " ++ circuitName ++ ".ngd")
       system ("map -intstyle silent -w -p " ++ partName ++ 
               mapEffortArgs effort ++
               " -o " ++ circuitName ++ "_map.ncd " ++ 
               circuitName ++ ".ngd " ++ circuitName ++ ".pcf")
       system ("par -w -intstyle silent " ++ parEffortArgs effort ++  " " ++ circuitName ++ "_map.ncd " ++ circuitName ++ ".ncd " ++ circuitName ++ ".pcf")
       reportPAR circuitName
       setCurrentDirectory ".."
       return ()
    where
    partName = part ++ "-" ++ show speedGrade ++ package
    circuit = circuitName ++ "_" ++ part ++ "_" ++ show speedGrade ++ 
              "_" ++ package ++ "_" ++ showEffort effort ++
              "_" ++ show frequency ++ "MHz"
              

-------------------------------------------------------------------------------
-- -u : do not trim any unused logic/signals

mapEffortArgs :: Effort -> String
mapEffortArgs Balanced = " -logic_opt off -ol high -t 1 -xt 0 -register_duplication off -global_opt off -mt off -ir off -pr off -lc off -power off -u"
mapEffortArgs HighEffort = " -logic_opt off -ol high -xe n -t 1 -xt 0 -register_duplication off -global_opt off -mt 2 -ir off -pr b -lc off -power off -u "
mapEffortArgs FastRuntime = " -logic_opt off -ol std -t 1 -xt 0 -register_duplication off -global_opt off -mt 2 -ir all -pr off -lc off -power off -u "

-------------------------------------------------------------------------------

parEffortArgs :: Effort -> String
parEffortArgs Balanced = " -ol high -mt off"
parEffortArgs HighEffort = " -ol high -xe n -mt 4"
parEffortArgs FastRuntime = " -ol std -mt 2"

-------------------------------------------------------------------------------


xstOptions :: String -> String -> [String]
xstOptions circuit part
  = ["set -xsthdpdir \"xst\"",
     "run",
     "-ifn " ++ circuit ++ ".prj",
     "-ifmt mixed",
     "-ofn " ++ circuit,
     "-ofmt NGC",
     "-p " ++ part,
     "-top " ++ circuit,
     "-opt_mode Speed",
     "-opt_level 2",
     "-register_balancing Yes",
     "-move_first_stage YES",
     "-move_last_stage YES",
     "-iob True"
    ]

-------------------------------------------------------------------------------

reportPAR :: String -> IO ()
reportPAR circuit
  = do parFile <- readFile (circuit++".par")
       let maybeResults = findPARresults (lines parFile)
           Just results = maybeResults
       if maybeResults == Nothing then
           putStrLn "Error parsing PAR results."
        else
           putStrLn (unlines results)  

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


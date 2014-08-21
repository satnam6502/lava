-------------------------------------------------------------------------------
--- $Id: Implement.hs#2 2010/09/30 20:03:24 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where
import System.Environment
import Lava.ISE

-------------------------------------------------------------------------------

main :: IO ()
main 
  = do args <- getArgs
       -- putStrLn (show args)
       let design:part:package:speed:effort:freqs = args
       -- putStrLn ("Speed: " ++ show ((read speed)::Int))
       -- putStrLn ("Effort: " ++ show ((read effort)::Effort))
       --putStrLn ("Frequency: " ++ show ((read freq)::Int))
       if length args >= 6 then
         sequence_ [implement design part package (read speed) (read effort) (read freq) | freq <- freqs]
         else
           error "implement <design name> <part> <package> <speed> <effort> <frequency>"

-------------------------------------------------------------------------------

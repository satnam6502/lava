-------------------------------------------------------------------------------
--- $Id: RPM.hs#4 2010/09/21 17:21:45 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.RPM
where
import Lava.Netlist

-------------------------------------------------------------------------------

showRLOC :: XilinxArchitecture -> (Int, Int) -> String
showRLOC Virtex2 (x, y)
   = rloc
     where
     rloc = "R" ++ show (-(y `div` 2)) ++ "C" ++ show (x `div` 2) ++
            ".S" ++ show ((x+1) `mod` 2) 

showRLOC Virtex4 (x,y)
    = "x" ++ show x ++ "y" ++ show (y `div` 2)

showRLOC Virtex5 (x,y)
    = "x" ++ show x ++ "y" ++ show (y `div` 2)
      
showRLOC Virtex6 (x,y)
    = "x" ++ show x ++ "y" ++ show (y `div` 4)

-------------------------------------------------------------------------------

-- Virtex-4
-- SLICE_X23Y92.F (lower cell)
-- SLICE_X23Y92.G (upper cell)

-------------------------------------------------------------------------------


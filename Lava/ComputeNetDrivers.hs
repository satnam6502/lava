-------------------------------------------------------------------------------
--- $Id: ComputeNetDrivers.hs#3 2010/10/07 16:17:27 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.ComputeNetDrivers
where

import Data.Array
import Data.Array.ST
import Control.Monad.State
import Control.Monad.ST
import Lava.Netlist

------------------------------------------------------------------------------

computeNetDrivers :: Netlist -> Netlist
computeNetDrivers netlist
  = netlist{netDrivers = runST (computeNetDrivers' netlist)}
 
-------------------------------------------------------------------------------

computeNetDrivers' :: Netlist -> ST s (Array Int DrivenPorts)
computeNetDrivers' netlist
  = do driven <- newArray (0, n-1) [] :: ST s (STArray s Int DrivenPorts)
       mapM_ (addDrivenNets driven) (instances netlist)
       freeze driven
    where
    n = netCount netlist

-------------------------------------------------------------------------------

addDrivenNets :: STArray s Int DrivenPorts -> Instance -> ST s ()
addDrivenNets drivenPorts inst
  = case component inst of
      Lut1 _ i0 o _ -> do pushDriver drivenPorts i0 "i0" instName
                          pushDriver drivenPorts o "o" instName
      Lut2 _ i0 i1 o _ -> do pushDriver drivenPorts i0 "i0" instName
                             pushDriver drivenPorts i1 "i1" instName
                             pushDriver drivenPorts o "o" instName
      Lut3 _ i0 i1 i2 o _ -> 
        do pushDriver drivenPorts i0 "i0" instName
           pushDriver drivenPorts i1 "i1" instName
           pushDriver drivenPorts i2 "i2" instName
           pushDriver drivenPorts o "o" instName
      Lut4 _ i0 i1 i2 i3 o _ -> 
        do pushDriver drivenPorts i0 "i0" instName
           pushDriver drivenPorts i1 "i1" instName
           pushDriver drivenPorts i2 "i2" instName
           pushDriver drivenPorts i3 "i3" instName
           pushDriver drivenPorts o "o" instName
      Lut5 _ i0 i1 i2 i3 i4 o _ -> 
        do pushDriver drivenPorts i0 "i0" instName
           pushDriver drivenPorts i1 "i1" instName
           pushDriver drivenPorts i2 "i2" instName
           pushDriver drivenPorts i3 "i3" instName
           pushDriver drivenPorts i4 "i4" instName
           pushDriver drivenPorts o "o" instName
      Lut6 _ i0 i1 i2 i3 i4 i5 o _ -> 
        do pushDriver drivenPorts i0 "i0" instName
           pushDriver drivenPorts i1 "i1" instName
           pushDriver drivenPorts i2 "i2" instName
           pushDriver drivenPorts i3 "i3" instName
           pushDriver drivenPorts i4 "i4" instName
           pushDriver drivenPorts i4 "i5" instName
           pushDriver drivenPorts o "o" instName
      PrimitiveGate inputs outputs
        -> do sequence_ [pushDriver drivenPorts n p instName | (p, n) <- inputs]
              sequence_ [pushDriver drivenPorts n p instName | (p, n) <- outputs]
 
   where
   instName = componentName inst ++ "_" ++ show (instanceNumber inst)

-------------------------------------------------------------------------------

pushDriver :: STArray s Int DrivenPorts
              -> Int -> String -> String -> ST s ()
pushDriver drivenArray i port inst
  = do driven <- readArray drivenArray i
       writeArray drivenArray i ((port, inst) : driven)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--- $Id: Instantiate.hs#1 2010/09/29 13:30:04 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Instantiate (instantiate)
where

import Control.Monad.State
import Lava.ComputeNetlist
import Lava.Netlist

-------------------------------------------------------------------------------

instantiate :: String -> Out () -> [(String, Bit)] -> [String] -> Out [Bit]
instantiate name circuit args outputs
  = do netlist <- get
       let sm = subCircuits netlist
           nc = netCount netlist
           ic = instCount netlist
       cn <- findSubCircuit name (xilinxArchitecture netlist) circuit sm
       let sz = if layout cn == [] then
                  Nothing
                else
                  Just (sizeOfLayout (head (layout cn)))
           Just (w, h) = sz
           placement = if sz /= Nothing && layoutNesting netlist > 0 then
                         Placed
                       else
                         Unplaced
       when (layoutNesting netlist > 0 && placement == Placed) $
         pushLayout (Tile (w,h) ic)
       addInstance (Instance (PrimitiveGate args (zip outputs [nc..]))
                    name ic placement sz)
       incrementNetCount (length outputs)
       incrementInstCount
       return [nc .. nc+length outputs-1]

-------------------------------------------------------------------------------

findSubCircuit :: String -> XilinxArchitecture ->
                   Out () -> [Netlist] -> Out Netlist
findSubCircuit name arch circuit []
  = do addSubCircuit circuitNL
       return circuitNL
    where
    circuitNL = computeNetlist name arch circuit
findSubCircuit name arch circuit (c:cs)
  = if name == circuitName c then
      return c
     else
      findSubCircuit name arch circuit  cs

-------------------------------------------------------------------------------

addSubCircuit :: Netlist -> Out ()
addSubCircuit subcir
  = do netlist <- get
       put netlist{subCircuits = subcir : subCircuits netlist}

-------------------------------------------------------------------------------

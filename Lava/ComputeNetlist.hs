-------------------------------------------------------------------------------
--- $Id: ComputeNetlist.hs#14 2010/10/06 10:24:29 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.ComputeNetlist (-- * Generating a Lava netlist
                            computeNetlist,
                            preLayoutNetlist
                            )
where

import Control.Monad.State

import Lava.ApplyLayout
import Lava.Gates
import Lava.Netlist

-------------------------------------------------------------------------------

computeNetlist :: String -> XilinxArchitecture -> Out () -> Netlist
computeNetlist name architecture circuitPrePowerAndGround
  = if nesting /= 0 then
      error ("Final layout nesting is not zero (" ++ show nesting ++ ")")
    else
      if length l > 1 then
        error ("Currently Lava only supports one top level layout group but has computed " ++ show (length l))
      else
        applyLayout netlist
    where
    netlist = preLayoutNetlist name architecture circuit 
    nesting = layoutNesting netlist
    l = layout netlist
    circuit = do wireUpPowerAndGround
                 circuitPrePowerAndGround
 
-------------------------------------------------------------------------------

preLayoutNetlist :: String -> XilinxArchitecture -> Out () -> Netlist
preLayoutNetlist name architecture circuit
  = execState circuit (Netlist name [] [] [] 0 0 [] 0 undefined Nothing
                       architecture [] undefined)

-------------------------------------------------------------------------------

wireUpPowerAndGround :: Out ()
wireUpPowerAndGround
  = do v0 <- gnd
       v1 <- vcc
       return ()

-------------------------------------------------------------------------------

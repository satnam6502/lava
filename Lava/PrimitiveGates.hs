-------------------------------------------------------------------------------
--- $Id: PrimitiveGates.hs#4 2010/09/29 13:30:04 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.PrimitiveGates (module Lava.PrimitiveGates)
where
import Control.Monad.State
import Lava.Netlist

-- * Adding new primitive gates to the Lava system

-------------------------------------------------------------------------------
-- | 'primitiveGate' adds a primitive gate 

primitiveGate :: String -- ^ The name of the component
                 -> [(String, Bit)] -- ^ name of input ports with argument nets
                 -> [String]  -- ^ name of output ports
                 -> Maybe (Int, Int) -- ^ optional size information for layout
                 -> Out [Bit] -- ^ a list of output nets from this component
primitiveGate gateName inputPorts outputNames maybeSize
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if maybeSize /= Nothing && layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           l = layout state
           l' = if layoutNesting state > 0 && placement == Placed then
                  Tile (w,h) instNr : l
                else
                  l
           outputs = zip outputNames [oNet..]
           newInst = Instance 
                       (PrimitiveGate inputPorts outputs) 
                       gateName instNr placement maybeSize
       put state{instances = newInst:insts, 
                 netCount = oNet + length outputNames, 
                 instCount = instNr +1,
                 layout = l'}
       return [oNet..oNet+length outputNames-1]
    where
    Just (w, h) = maybeSize

-------------------------------------------------------------------------------

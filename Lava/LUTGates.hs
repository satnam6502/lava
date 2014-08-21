-------------------------------------------------------------------------------
--- $Id: LUTGates.hs#5 2010/09/29 15:05:17 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.LUTGates (-- * Functions for defining new LUT-based gates
                      lut1gate, lut2gate, lut2gate_l, 
                      lut3gate, lut4gate, lut5gate, lut6gate)
where
import Control.Monad.State
import Lava.Netlist


-------------------------------------------------------------------------------
-- | Implements a user-defined 1 input combinational gate

lut1gate :: (Bool -> Bool) -> String -> Bit -> Out Bit
lut1gate gateOperation comment i0
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut1 opBits i0 oNet comment) 
                               "lut1" instNr placement cSize
       put (state{instances = newInst:insts, 
                  netCount = oNet+1, 
                  instCount = instNr +1,
                  layout = l'})
       return oNet 
    where
    opBits = map boolToInt [gateOperation False, gateOperation True]

-------------------------------------------------------------------------------

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

-------------------------------------------------------------------------------
-- | Implements a user defined two input combinational gate

lut2gate :: (Bool -> Bool -> Bool) -> String -> (Bit, Bit) -> Out Bit
lut2gate gateOperation comment (i0, i1)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut2 opBits i0 i1 oNet comment) 
                               "lut2" instNr placement cSize
       put (state{instances = newInst:insts, 
                  netCount = oNet+1, 
                  instCount = instNr +1,
                  layout = l'})
       return oNet
    where
    opBits = map boolToInt [gateOperation False False,
                            gateOperation True  False,
                            gateOperation False True,
                            gateOperation True  True]

-------------------------------------------------------------------------------
-- | Implements a local user defined two input combinational gate

lut2gate_l :: (Bool -> Bool -> Bool) -> String -> (Bit, Bit) -> Out Bit
lut2gate_l gateOperation comment (i0, i1)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut2_l opBits i0 i1 oNet comment) 
                               "lut2_l" instNr placement cSize
       put (state{instances = newInst:insts, 
                  netCount = oNet+1, 
                  instCount = instNr +1,
                  layout = l'})
       return oNet
    where
    opBits = map boolToInt [gateOperation False False,
                            gateOperation True  False,
                            gateOperation False True,
                            gateOperation True  True]

-------------------------------------------------------------------------------
-- | Implements a user defined three input combinational gate

lut3gate :: (Bool -> Bool -> Bool -> Bool) -> String -> (Bit, Bit, Bit) 
             -> Out Bit
lut3gate gateOperation comment (i0, i1, i2)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut3 opBits i0 i1 i2 oNet comment) 
                               "lut3" instNr placement cSize
       put state{instances = newInst:insts, 
                 netCount = oNet+1, 
                 instCount = instNr +1,
                 layout = l'}
       return oNet
    where
    opBits = map boolToInt [gateOperation i0 i1 i2 | [i0, i1, i2] <- boolVecs 3]
 
-------------------------------------------------------------------------------
-- | Implements a user defined four input combinational gate

lut4gate :: (Bool -> Bool -> Bool -> Bool -> Bool) -> String -> 
            (Bit, Bit, Bit, Bit) -> Out Bit
lut4gate gateOperation comment (i0, i1, i2, i3)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut4 opBits i0 i1 i2 i3 oNet comment) 
                               "lut4" instNr placement cSize
       put state{instances = newInst:insts, 
                 netCount = oNet+1, 
                 instCount = instNr +1,
                 layout = l'}
       return oNet
    where
    opBits = map boolToInt [gateOperation i0 i1 i2 i3 | 
                            [i0, i1, i2, i3] <- boolVecs 4]

-------------------------------------------------------------------------------
-- | Implements a user defined five input combinational gate

lut5gate :: (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> String -> 
            (Bit, Bit, Bit, Bit, Bit) -> Out Bit
lut5gate gateOperation comment (i0, i1, i2, i3, i4)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut5 opBits i0 i1 i2 i3 i4 oNet comment) 
                               "lut5" instNr placement cSize
       put state{instances = newInst:insts, 
                 netCount = oNet+1, 
                 instCount = instNr +1,
                 layout = l'}
       return oNet
    where
    opBits = map boolToInt [gateOperation i0 i1 i2 i3 i4 | 
                            [i0, i1, i2, i3, i4] <- boolVecs 5]

-------------------------------------------------------------------------------
-- | Implements a user defined six input combinational gate

lut6gate :: (Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> String -> 
            (Bit, Bit, Bit, Bit, Bit, Bit) -> Out Bit
lut6gate gateOperation comment (i0, i1, i2, i3, i4, i5)
  = do state <- get
       let insts = instances state
           oNet = netCount state
           instNr = instCount state
           placement = if layoutNesting state > 0 then
                         Placed
                       else
                         Unplaced
           cSize = if layoutNesting state > 0 then
                     Just (1,1)
                   else
                     Nothing
           l = layout state
           l' = if layoutNesting state > 0 then
                  Tile (1,1) instNr : l
                else
                  l
           newInst = Instance (Lut6 opBits i0 i1 i2 i3 i4 i5 oNet comment) 
                               "lut6" instNr placement cSize
       put state{instances = newInst:insts, 
                 netCount = oNet+1, 
                 instCount = instNr +1,
                 layout = l'}
       return oNet
    where
    opBits = map boolToInt [gateOperation i0 i1 i2 i3 i4 i5 | 
                            [i0, i1, i2, i3, i4, i5] <- boolVecs 6]

-------------------------------------------------------------------------------

boolVecs :: Int -> [[Bool]]
boolVecs n
  = [numToBoolVec n i | i <- [0..2^n-1]]


-------------------------------------------------------------------------------

numToBoolVec 0 0 = []
numToBoolVec n i
  = lsb : numToBoolVec (n-1) (i `div` 2)
    where
    lsb = if i `mod` 2 == 0 then
            False
          else
            True

-------------------------------------------------------------------------------

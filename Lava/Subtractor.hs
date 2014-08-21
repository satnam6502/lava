-----------------------------------------------------------------------------
--- $Id: Subtractor.hs#1 2010/10/01 19:10:34 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Subtractor
where
import Lava
import Lava.Virtex6
import Lava.OneBitSubtractor

-------------------------------------------------------------------------------
-- | A subtactor circuit

subtractor :: (Bit, ([Bit], [Bit])) -> Out ([Bit], Bit)
subtractor (cin, (a,b))
  = col oneBitSubtractor (cin, zip a b)

-------------------------------------------------------------------------------

subtractorNoCarryIn :: ([Bit], [Bit]) -> Out ([Bit], Bit)
subtractorNoCarryIn (a, b)
  = subtractor (one, (a,b))

-------------------------------------------------------------------------------

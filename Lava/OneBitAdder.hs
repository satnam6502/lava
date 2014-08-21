------------------------------------------------------------------------------
--- $Id: OneBitAdder.hs#1 2010/10/01 19:10:34 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.OneBitAdder
where
import Lava
import Lava.Virtex6

-------------------------------------------------------------------------------

oneBitAdder :: (Bit, (Bit, Bit)) -> Out (Bit, Bit)
oneBitAdder (cin, (a, b))
  = overlayTile $ do
      part_sum <- xor2 (a, b)
      sum <- xorcy (part_sum, cin)
      cout <- muxcy (part_sum, (a, cin))
      return (sum, cout)

-------------------------------------------------------------------------------

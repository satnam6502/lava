------------------------------------------------------------------------------
--- $Id: OneBitSubtractor.hs#1 2010/10/01 19:10:34 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.OneBitSubtractor
where
import Lava
import Lava.Virtex6

------------------------------------------------------------------------------

oneBitSubtractor :: (Bit, (Bit, Bit)) -> Out (Bit, Bit)
oneBitSubtractor (cin, (a, b))
  = do overlayTile $ 
         do part_sum <- xnor2 (a, b)
            sum <- xorcy (part_sum, cin)
            cout <- muxcy (part_sum, (a, cin))
            return (sum, cout)

-------------------------------------------------------------------------------

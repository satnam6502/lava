-----------------------------------------------------------------------------
--- $Id: Comparator.hs#1 2010/10/01 19:10:34 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Comparator
where
import Lava
import Lava.Virtex6
import Lava.Subtractor

-------------------------------------------------------------------------------
-- | The comparator returns '1' if b >= a, '0' otherwise.

comparator :: ([Bit], [Bit]) -> Out Bit
comparator 
  = subtractorNoCarryIn >-> projectSnd 

-------------------------------------------------------------------------------

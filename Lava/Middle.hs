{-# LANGUAGE DoRec #-}

-------------------------------------------------------------------------------
--- $Id: Middle.hs#3 2010/09/30 16:19:34 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Middle (middle)
where
import Lava.Netlist
import Lava.Combinators

-------------------------------------------------------------------------------
-- | Place components in a horizontal middle arrangement

middle :: (a -> Out c) -> ((c, d) -> Out e) -> (b -> Out d) ->
          (a, b) -> Out e
middle r s t (a, b)
  = do rec { (x, (z, y)) <- hpar2 r (hpar2 s t) (a, ((x,y), b)) }
       return z

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--- $Id: OverlayTile.hs#1 2010/09/28 17:03:48 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.OverlayTile (overlayTile)
where
import Control.Monad.State
import Lava.Netlist

-------------------------------------------------------------------------------
-- | overlayTile takes a circuit instantiation block and overlays all the
--   the instantions.

overlayTile :: Out a -> Out a
overlayTile circuit 
  = do incrementLayoutNesting
       pushLayout BeginOverlayTile
       r <- circuit
       decrementLayoutNesting
       performOverlay
       return r

-------------------------------------------------------------------------------

performOverlay :: Out ()
performOverlay
  = do l1 <- popLayout
       when (l1 /= BeginOverlayTile) $ -- There is at least one element in tile
         do l2 <- popLayout
            if l2 /= BeginOverlayTile then -- Two tiles to combine
              do let (aW, aH) = sizeOfLayout l1
                     (bW, bH) = sizeOfLayout l2 
                 pushLayout (Overlay (aW `max` bW, aH `max` bH) l1 l2)
                 performOverlay
             else -- Just one tile so put it back
                pushLayout l1

-------------------------------------------------------------------------------


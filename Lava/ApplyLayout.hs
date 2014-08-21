-------------------------------------------------------------------------------
--- $Id: ApplyLayout.hs#7 2010/10/01 19:17:36 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.ApplyLayout
where 
import Data.Array.ST
import Control.Monad.State
import Data.Array.Unboxed
import Control.Monad.ST
import Lava.Netlist

-------------------------------------------------------------------------------

applyLayout :: Netlist -> Netlist
applyLayout state
  = state{computedShifts = runST (applyLayout' state)}
 
-------------------------------------------------------------------------------

type MutablePositions s = STUArray s Int Int

-------------------------------------------------------------------------------

applyLayout' :: Netlist -> ST s (Array Int Int, Array Int Int)
applyLayout' state
  = do xs <- newArray (0, n-1) 0 :: ST s (STUArray s Int Int)
       ys <- newArray (0, n-1) 0 :: ST s (STUArray s Int Int)
       applyLayoutElements state (layout state) xs ys 
       xsf <- freeze xs
       ysf <- freeze ys
       return (xsf, ysf)
    where
    n = instCount state 

-------------------------------------------------------------------------------

applyLayoutElements :: Netlist -> [Layout] ->
                       MutablePositions s -> MutablePositions s ->
                       ST s ()
applyLayoutElements state [] xs ys = return ()
applyLayoutElements state (l:ls) xs ys 
  = do applyLayoutElement  state l  xs ys (0, 0)
       applyLayoutElements state ls xs ys

-------------------------------------------------------------------------------

applyLayoutElement :: Netlist ->  Layout -> MutablePositions s ->
                      MutablePositions s -> (Int, Int) -> ST s ()
applyLayoutElement state (Beside _ aTile bTile) xs ys dxy@(dx, dy)
  = do applyLayoutElement state aTile xs ys dxy
       applyLayoutElement state bTile xs ys (dx+aW, dy)
    where
    (aW, aH) = sizeOfLayout aTile
applyLayoutElement state (Below _ aTile bTile) xs ys dxy@(dx, dy)
  = do applyLayoutElement state aTile xs ys dxy
       applyLayoutElement state bTile xs ys (dx, dy+aH)
    where
    (aW, aH) = sizeOfLayout aTile
applyLayoutElement state (Overlay _ aTile bTile) xs ys dxy
  = do applyLayoutElement state aTile xs ys dxy
       applyLayoutElement state bTile xs ys dxy
applyLayoutElement state (Tile _ i) xs ys dxy
  = translateInstance i dxy xs ys
applyLayoutElement state (Space _) xs ys dxy = return ()
applyLayoutElement state (ConditionalShift condShift l)
                   xs ys dxy
  = do applyLayoutElement state l xs ys dxy
       let n = instCount state
       sequence_ [condTranslateInstance condShift i dxy xs ys | i <- [0..n-1]]

-------------------------------------------------------------------------------

translateInstances :: (Int, Int) -> (Int, Int) ->
                      MutablePositions s -> MutablePositions s -> ST s ()
translateInstances (a,b) dxy xs ys 
   = sequence_ [translateInstance i dxy xs ys | i <- [a..b]]

-------------------------------------------------------------------------------

translateInstance :: Int -> (Int, Int) -> 
                     MutablePositions s -> MutablePositions s ->
                     ST s ()
translateInstance i (dx, dy) xs ys
  = do x <- readArray xs i
       writeArray xs i (x+dx)
       y <- readArray ys i
       writeArray ys i (y+dy)

-------------------------------------------------------------------------------

condTranslateInstance :: CondShiftFn ->
                         Int -> (Int, Int) -> 
                         MutablePositions s -> MutablePositions s ->
                         ST s ()
condTranslateInstance (CondShiftFn (xcond, xfn) (ycond, yfn)) i (dx, dy) xs ys
  = do x <- readArray xs i
       writeArray xs i (if xcond x then xfn (x+dx) else x+dx)
       y <- readArray ys i
       writeArray ys i (if ycond y then xfn (y+dy) else y+dy)

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--- $Id: Combinators.hs#13 2010/10/07 16:17:27 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Combinators (module Lava.Combinators)
where
import Control.Monad.State
import Lava.Netlist
import Lava.Utils

-- * Lava Combinators

-------------------------------------------------------------------------------

infixr 5 >->
infixr 5 >|>
infixr 5 >=>
infixr 5 ->-

-- ** Serial composition combinators

-------------------------------------------------------------------------------
-- | Serial composition with no layout

(->-) :: (a -> Out b) -> (b -> Out c) -> a -> Out c
(->-) circuit1 circuit2 input1
  = do input2 <- circuit1 input1
       circuit2 input2

-------------------------------------------------------------------------------
-- | Serial composition with horizontal left to right layout

(>->) :: (a -> Out b) -> (b -> Out c) -> a -> Out c
(>->) circuit1 circuit2 input1
  = do preState <- get
       let l0 = length (layout preState)
       incrementLayoutNesting
       intermediateResult <- circuit1 input1
       r <- circuit2 intermediateResult
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
       when (l1 - l0 >= 2) $
         put state{layout = Beside (aW+bW, aH `max` bH) aTile bTile : lrest}
       return r

-------------------------------------------------------------------------------
-- | Serial composition with mid-horizontal left to right layout

(>=>) :: (a -> Out b) -> (b -> Out c) -> a -> Out c
(>=>) circuit1 circuit2 input1
  = do preState <- get
       let l0 = length (layout preState)
       incrementLayoutNesting
       intermediateResult <- circuit1 input1
       r <- circuit2 intermediateResult
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
           vGap = (aH - bH) `div` 2
           bTile' = Below (bW, bH+vGap) (Space (0, vGap)) bTile
       when (l1 - l0 >= 2) $
         put state{layout = Beside (aW+bW, aH `max` bH) aTile bTile' : lrest}
       return r

-------------------------------------------------------------------------------
-- | Serial composition with overly layout

(>|>) :: (a -> Out b) -> (b -> Out c) -> a -> Out c
(>|>) circuit1 circuit2 input1
  = do preState <- get
       let l0 = length (layout preState)
       incrementLayoutNesting
       intermediateResult <- circuit1 input1
       r <- circuit2 intermediateResult
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
       when (l1 - l0 >= 2) $
         put state{layout = Overlay (aW `max` bW, aH `max` bH) aTile bTile : lrest}
       return r

-------------------------------------------------------------------------------

-- ** Conditional shift for obstacle avoidance

condShift ::    (Int -> Bool, Int -> Int) 
             -> (Int -> Bool, Int -> Int)
             -> Out ()
condShift xshift yshift
  = do netlist <- get
       l <- popLayout
       pushLayout (ConditionalShift (CondShiftFn xshift yshift) l)
             
-------------------------------------------------------------------------------

-- ** Parallel composition combinators

-------------------------------------------------------------------------------
-- | Repeated serial composition (left to right)

hRepN :: Int -> (a -> Out a) -> a -> Out a
hRepN 1 circuit = circuit
hRepN n circuit
  = circuit >-> (hRepN (n-1) circuit)

-------------------------------------------------------------------------------
-- | Vertical parallel composition of two circuits

par2 :: (a -> Out c) -> (b -> Out d) -> (a, b) -> Out (c, d)
par2 circuit1 circuit2 (a, b)
  = do preState <- get
       let l0 = length (layout preState)
       incrementLayoutNesting
       c <- circuit1 a
       d <- circuit2 b
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
       when (l1 - l0 >= 2) $
         put state{layout = Below (aW `max` bW, aH+bH) aTile bTile : lrest}
       return (c, d)

-------------------------------------------------------------------------------
-- | Vertical map of a circuit

maP :: (a -> Out b) -> [a] -> Out [b]
maP circuit [] = return []
maP circuit (x:xs)
  = do (y, ys) <- par2 circuit (maP circuit) (x, xs)
       return (y:ys)

-------------------------------------------------------------------------------
-- | 'mapPair' maps a circuit over adajcent pairs of elements in a list

mapPair :: ((a, a) -> Out a) -> [a] -> Out [a]
mapPair circuit l | odd (length l)
  = do r <- (chopPair >-> maP circuit) (init l)
       return (r ++ [last l])
mapPair circuit l = (chopPair >-> maP circuit) l

-------------------------------------------------------------------------------
-- | Horizontal parallel composition of two circuits

hpar2 :: (a -> Out c) -> (b -> Out d) -> (a, b) -> Out (c, d)
hpar2 circuit1 circuit2 (a, b)
  = do preState <- get
       let l0 = length (layout preState) 
       incrementLayoutNesting
       c <- circuit1 a
       d <- circuit2 b
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
       when (l1 - l0 >= 2) $
         put state{layout = Beside (aW + bW, aH `max` bH) aTile bTile : lrest}
       return (c, d)

-------------------------------------------------------------------------------
-- | Horizontal map of a circuit

hmaP :: (a -> Out b) -> [a] -> Out [b]
hmaP circuit [] = return []
hmaP circuit (x:xs)
  = do (y, ys) <- hpar2 circuit (hmaP circuit) (x, xs)
       return (y:ys)

-------------------------------------------------------------------------------
-- | Parallel composition of two circuit which have overlaid layout

par2Overlay :: (a -> Out c) -> (b -> Out d) -> (a, b) -> Out (c, d)
par2Overlay circuit1 circuit2 (a, b)
  = do preState <- get
       let l0 = length (layout preState) 
       incrementLayoutNesting
       c <- circuit1 a
       d <- circuit2 b
       decrementLayoutNesting
       state <- get
       let l = layout state
           l1 = length l
           bTile:aTile:lrest = l
           (aW, aH) = sizeOfLayout aTile
           (bW, bH) = sizeOfLayout bTile
       when (l1 - l0 >= 2) $
         put state{layout = Overlay (aW `max` bW, aH `max` bH) aTile bTile : lrest}
       return (c, d)

-------------------------------------------------------------------------------
-- | Parallel composition of three circuit which have overlaid layout

par3Overlay :: (a -> Out ao) -> (b -> Out bo) -> (c -> Out co) 
                -> (a, b, c)
                -> Out (ao, bo, co)
par3Overlay circuit1 circuit2 circuit3 (a, b, c)
  = do ((ao,bo),co) <- par2Overlay (par2Overlay circuit1 circuit2) circuit3
                       ((a,b), c)
       return (ao, bo, co)

-------------------------------------------------------------------------------
-- | Horizontal parallel composition of a list of circuits

hpar :: [a -> Out b] -> [a] -> Out [b]
hpar [] [] = return []
hpar (c:cs) (i:is)
  = do (x,y) <- hpar2 c (hpar cs) (i, is)
       return (x:y)
    
-------------------------------------------------------------------------------
-- | Horizontal repeated parallel composition of a circuit

hparN :: Int -> (a -> Out b) -> [a] -> Out [b]
hparN n circuit = hpar (replicate n circuit)

-- ** Wiring combinators

-------------------------------------------------------------------------------
-- | Splits a wire into two

fork2 :: a -> Out (a, a)
fork2 a = return (a, a)

-------------------------------------------------------------------------------
-- | Converts a two element list into a pair

listToPair :: [a] -> Out (a, a)
listToPair [a, b] = return (a, b)
listToPair other 
  = error ("listToPair called with a list of length " ++ show (length other))

-------------------------------------------------------------------------------
-- | Converts a par into a list containing two elements

pairToList :: (a, a) -> Out [a]
pairToList (a, b) = return [a, b]

-------------------------------------------------------------------------------
-- | Takes a pair of lists and returns a zipped list of pairs

ziP :: ([a], [b]) -> Out [(a,b)]
ziP (a,b) = return (zip a b)

-------------------------------------------------------------------------------
-- | Takes a list of pairs and unzips it into a pair of  lists

unziP :: [(a,b)] -> Out ([a], [b])
unziP list = return (unzip list)

-------------------------------------------------------------------------------
-- | Takes a list containing two elements and returns a list of lists
--   where each element is a two element list

zipList :: [[a]] -> Out [[a]]
zipList [[], _] = return []
zipList [_, []] = return []
zipList [a:as, b:bs] 
  = do rest <- zipList [as, bs]
       return ([a,b] : rest) 

-------------------------------------------------------------------------------
-- | Undo the zipList operation

unzipList :: [[a]] -> Out [[a]]
unzipList list = return [map fstListPair list, map sndListPair list]

-------------------------------------------------------------------------------

fstListPair :: [a] -> a
fstListPair [a, _] = a 

-------------------------------------------------------------------------------

sndListPair :: [a] -> a
sndListPair [_, b] = b 

-------------------------------------------------------------------------------
-- | This makes pairs out of consequetive members of an even length list.

pair :: [a] -> Out [[a]]
pair [] = return []
pair lst | odd (length lst) 
  = error ("pair given odd length list of size " ++ show (length lst))
pair (a:b:rest) 
  = do rest <- pair rest
       return ([a,b]:rest)

-------------------------------------------------------------------------------
-- | Takes a list of pairs and returns a flattend list

unpair :: [[a]] -> Out [a]
unpair list = return (concat list)

-------------------------------------------------------------------------------
-- | 'halveListToPair' will take a list and return a pair containing the
--   two halves.

halveListToPair :: [a] -> ([a], [a])
halveListToPair l
  = (take n l, drop n l)
    where
    n = length l `div` 2

-------------------------------------------------------------------------------
-- | Tales a list and returns a pair containing the two halves of the list

halve :: [a] -> Out ([a], [a])
halve l = return (halveListToPair l)
  
-------------------------------------------------------------------------------
-- | Take a pair containing two list halves and undoes the halve

unhalve :: ([a], [a]) -> Out [a]
unhalve (a, b) = return (a ++ b)

-------------------------------------------------------------------------------
-- | Halves the input list into a list containign the two halves

halveList :: [a] -> Out [[a]]
halveList l
  = return [take n l, drop n l]
    where
    n = length l `div` 2
 
-------------------------------------------------------------------------------
-- | Undoes halveList

unhalveList :: [[a]] -> Out [a]
unhalveList [a, b] = return (a ++ b)

-------------------------------------------------------------------------------
-- | Chops a list into chunks

chop :: Int -> [a] -> Out [[a]]
chop n l = return (chopList n l)

-------------------------------------------------------------------------------
-- | Chops a list into chunks formed as pairs

chopPair :: [a] -> Out [(a, a)]
chopPair = chop 2 >-> maP listToPair 

-------------------------------------------------------------------------------
-- | Takes a list of lists and returns their concatenation

concaT :: [[a]] -> Out [a]
concaT list = return (concat list)

-------------------------------------------------------------------------------
-- | Applies a circuit to the first halve of a list

fstList :: ([a] -> Out [a]) -> [a] -> Out [a]
fstList f = halve >-> fsT f >-> unhalve

-------------------------------------------------------------------------------
-- | Applies a circuit to the second halve of a list

sndList :: ([a] -> Out [a]) -> [a] -> Out [a]
sndList f = halve >-> snD f >-> unhalve

-------------------------------------------------------------------------------
-- | Applies a circuit to the first element of a pair

fsT :: (a -> Out b) -> (a, c) -> Out (b, c)
fsT f (a, b) 
  = do c <- f a
       return (c, b)

-------------------------------------------------------------------------------
-- | Applies a circuit to the second element of a pair

snD :: (b -> Out c) -> (a, b) -> Out (a, c)
snD f (a, b) 
  = do c <- f b
       return (a, c)

-------------------------------------------------------------------------------

projectFst :: (a, b) -> Out a
projectFst (a, b) = return a

-------------------------------------------------------------------------------

projectSnd :: (a, b) -> Out b
projectSnd (a, b) = return b

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reverses a list

reversE :: [a] -> Out [a]
reversE list = return (reverse list)

-------------------------------------------------------------------------------

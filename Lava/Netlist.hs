-------------------------------------------------------------------------------
--- $Id: Netlist.hs#14 2010/10/06 16:55:56 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Netlist (module Lava.Components,
                     module Lava.Netlist)
where

import Control.Monad.State

import Lava.Components
import Data.Array.Unboxed

-- * The Lava Netlist representation


-------------------------------------------------------------------------------

data Dir = To | Downto
           deriving (Eq, Show)

data NetType
  = BitType
  | BitVec Int Dir Int
  | ArrayType Int Dir Int NetType
  | NamedType String
    deriving (Eq, Show)

data PortDirection
  = InputPort
  | OutputPort
  | LocalInput
  | LocalOutput
    deriving (Eq, Show)

-------------------------------------------------------------------------------

data PortDeclaration = Port String PortDirection NetType [Int]
                       deriving (Eq, Show)

-------------------------------------------------------------------------------

data Position
  = NoPlacement
  | Unplaced
  | Placed
    deriving (Eq, Show)

-------------------------------------------------------------------------------

data Layout
  = Beside (Int, Int) Layout Layout
  | Below  (Int, Int) Layout Layout
  | Overlay (Int, Int) Layout Layout
  | Tile (Int, Int) Int -- (w,h) instNr
  | BeginOverlayTile
  | EmptyLayout
  | Space (Int, Int)
  | ConditionalShift CondShiftFn Layout
    deriving (Eq, Show)

-------------------------------------------------------------------------------

data CondShiftFn 
  = CondShiftFn (Int -> Bool, Int -> Int) (Int -> Bool, Int -> Int)

-------------------------------------------------------------------------------

instance Show CondShiftFn where
  show _ = "<cond shift>"

-------------------------------------------------------------------------------

instance Eq CondShiftFn where
  (==) _ _ = True

-------------------------------------------------------------------------------


sizeOfLayout :: Layout -> (Int, Int)
sizeOfLayout (Beside wh _ _) = wh
sizeOfLayout (Below wh _ _) = wh
sizeOfLayout (Overlay wh _ _) = wh
sizeOfLayout (Tile wh _) = wh
sizeOfLayout EmptyLayout = (0, 0)
sizeOfLayout (Space wh) = wh
sizeOfLayout (ConditionalShift _ l) = sizeOfLayout l
sizeOfLayout other = error ("sizeOfLayout: " ++ show other)

-------------------------------------------------------------------------------

data XilinxArchitecture
  = Virtex2
  | Virtex4
  | Virtex5
  | Virtex6
    deriving (Eq, Show)

-------------------------------------------------------------------------------

data Instance = Instance {component :: Component,
                          componentName :: String,
                          instanceNumber :: Int,
                          position :: Position,
                          componentSize :: Maybe (Int,Int)}
                deriving (Eq, Show)

-------------------------------------------------------------------------------

type DrivenPorts = [(String, String)]

-------------------------------------------------------------------------------

data Netlist = Netlist {circuitName :: String,
                        ports :: [PortDeclaration],
                        types :: [(String, NetType)],
                        instances :: [Instance],
                        netCount :: Int,
                        instCount :: Int,
                        layout :: [Layout],
                        layoutNesting :: Int,
                        computedShifts :: (Array Int Int, Array Int Int),
                        rlocOrigin :: Maybe (Int, Int),
                        xilinxArchitecture :: XilinxArchitecture,
                        subCircuits :: [Netlist],
                        netDrivers :: Array Int DrivenPorts
                       }

-------------------------------------------------------------------------------

setCircuitName :: String -> Out ()
setCircuitName name
  = do netlist <- get
       put netlist{circuitName = name}

-------------------------------------------------------------------------------

addInstance :: Instance -> Out ()
addInstance inst
  = do netlist <- get
       put netlist{instances = inst : instances netlist}

-------------------------------------------------------------------------------


getNetCount :: Out Int
getNetCount
  = do state <- get
       return (netCount state)

-------------------------------------------------------------------------------

getInstCount :: Out Int
getInstCount
  = do state <- get
       return (instCount state)

-------------------------------------------------------------------------------

incrementInstCount :: Out ()
incrementInstCount
  = do netlist <- get
       put netlist{instCount = instCount netlist + 1}

-------------------------------------------------------------------------------

incrementNetCount :: Int -> Out ()
incrementNetCount by
  = do netlist <- get
       put netlist{netCount = netCount netlist + by}

-------------------------------------------------------------------------------

getNewNet:: Out Bit
getNewNet
  = do n <- getNetCount
       incrementNetCount 1
       return n

-------------------------------------------------------------------------------

pushLayout :: Layout -> Out ()
pushLayout newLayoutElement
  = do state <- get
       let currentLayout = layout state
       put state {layout = newLayoutElement : currentLayout}

-------------------------------------------------------------------------------

popLayout :: Out Layout
popLayout
  = do state <- get
       let (l:ls) = layout state
       put state{layout = ls}
       return l

-------------------------------------------------------------------------------

incrementLayoutNesting :: Out ()
incrementLayoutNesting
  = do state <- get
       put state{layoutNesting = layoutNesting state + 1}

-------------------------------------------------------------------------------

decrementLayoutNesting :: Out ()
decrementLayoutNesting
  = do state <- get
       put state{layoutNesting = layoutNesting state - 1}

-------------------------------------------------------------------------------

setOrigin :: (Int, Int) -> Out ()
setOrigin (x, y)
  = do netlist <- get
       put netlist{rlocOrigin = Just (x, y)}

-------------------------------------------------------------------------------

type Out a = State Netlist a

-------------------------------------------------------------------------------

type Bit = Int
type BitVec = (Int, Dir, Int)

zero :: Bit
zero = 0

one :: Bit
one = 1

-------------------------------------------------------------------------------


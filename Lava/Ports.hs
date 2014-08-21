-------------------------------------------------------------------------------
--- $Id: Ports.hs#8 2010/10/12 16:50:38 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Ports (module Lava.Ports)
where
import Control.Monad.State
import Data.List
import Lava.Netlist
import Lava.PortRange

-- * Circuit input/output ports

-------------------------------------------------------------------------------
-- | 'inputPort' creates a single bit input port

inputPort :: String -> NetType -> Out Bit
inputPort name BitType
  = do state <- get
       let p = ports state
           oNet = netCount state
       put state{ports = Port name InputPort BitType [oNet] : p, 
                  netCount = oNet+1}
       return oNet

-------------------------------------------------------------------------------
-- | 'inputBitVec' creates a bit-vector input port

inputBitVec :: String -> NetType -> Out [Bit]
inputBitVec name bvt@(BitVec a dir b)
  = do state <- get
       let p = ports state
           oNet = netCount state
           oNets = [oNet + i | (p,i) <- zip (portRange a dir b) [0..]]
       put state{ports = Port name InputPort bvt oNets : p, 
            netCount = oNet + 1 + abs (a-b)}
       return oNets

-------------------------------------------------------------------------------
-- | 'inputArrayOfArray' creates an input array of arrays.

inputArrayOfArray :: String -> NetType -> Out [[Bit]]
inputArrayOfArray name t@(ArrayType al dir1 ah (BitVec bl dir2 bh))
  = error "inputArrayOfArray: array types must be named"
inputArrayOfArray name t@(NamedType typeName)
  = do netlist <- get
       let Just (ArrayType al dir1 ah (BitVec bl dir2 bh))
             = lookup typeName (types netlist)
       nets <- sequence [freshBitVec bl dir2 bh | i <- portRange al dir1 ah]
       put netlist{ports = Port name InputPort t (concat nets) : ports netlist,
                   netCount = netCount netlist
                              + (abs (ah-al)+1) * (abs (bh-bl)+1) }
       return nets

-------------------------------------------------------------------------------
-- | 'inputLocalArrayOfArray' creates an input array of arrays.

inputLocalArrayOfArray :: String -> NetType -> Out [[Bit]]
inputLocalArrayOfArray name t@(ArrayType al dir1 ah (BitVec bl dir2 bh))
  = error "inputArrayOfArray: array types must be named"
inputLocalArrayOfArray name t@(NamedType typeName)
  = do netlist <- get
       let Just (ArrayType al dir1 ah (BitVec bl dir2 bh))
             = lookup typeName (types netlist)
       nets <- sequence [freshBitVec bl dir2 bh | i <- portRange al dir1 ah]
       put netlist{ports = Port name LocalInput t (concat nets) : ports netlist,
                   netCount = netCount netlist
                              + (abs (ah-al)+1) * (abs (bh-bl)+1) }
       return nets

-------------------------------------------------------------------------------

freshBitVec :: Int -> Dir -> Int -> Out [Bit]
freshBitVec al dir ah
  = sequence [getNewNet | i <- portRange al dir ah]

-------------------------------------------------------------------------------

outputArrayOfArray :: String -> NetType -> [[Bit]] -> Out ()
outputArrayOfArray name t@(ArrayType al dir1 ah (BitVec bl dir2 bh)) nets
  = error "outputArrayOfArray: array types must be named"
outputArrayOfArray name t@(NamedType typeName) nets
  = do netlist <- get
       let Just (ArrayType al dir1 ah (BitVec bl dir2 bh))
              = lookup typeName (types netlist)
       put netlist{ports = Port name OutputPort t (concat nets) : 
                   ports netlist}

-------------------------------------------------------------------------------

outputLocalArrayOfArray :: String -> NetType -> [[Bit]] -> Out ()
outputLocalArrayOfArray name t@(ArrayType al dir1 ah (BitVec bl dir2 bh)) nets
  = error "outputArrayOfArray: array types must be named"
outputLocalArrayOfArray name t@(NamedType typeName) nets
  = do netlist <- get
       let Just (ArrayType al dir1 ah (BitVec bl dir2 bh))
              = lookup typeName (types netlist)
       put netlist{ports = Port name LocalOutput t (concat nets) : 
                   ports netlist}

-------------------------------------------------------------------------------
-- | 'inputBitVecLocal' creates a local bit-vector input port

inputBitVecLocal :: String -> NetType -> Out [Bit]
inputBitVecLocal name bvt@(BitVec a dir b)
  = do state <- get
       let p = ports state
           oNet = netCount state
           oNets = [oNet + i | (p,i) <- zip (portRange a dir b) [0..]]
       put (state{ports = Port name LocalInput bvt oNets : p, 
            netCount = oNet + 1 + abs (a-b)})
       return oNets

-------------------------------------------------------------------------------

-- | 'outputPort' creates a single bit output port

outputPort :: String -> NetType -> Bit -> Out ()
outputPort name BitType o
  = do state <- get
       let p = ports state
       put (state {ports = Port name OutputPort BitType [o] : p})

-------------------------------------------------------------------------------
-- | 'outputBitVec' creates a bit-vector output port

outputBitVec :: String -> NetType -> [Bit] -> Out ()
outputBitVec name bvt@(BitVec a dir b) o
  = do state <- get
       let p = ports state
       put (state {ports = Port name OutputPort bvt o : p})

-------------------------------------------------------------------------------
-- | 'outputBitVecKept' creates a bit-vector local signal with a
--   KEEP attribute set

outputBitVecLocal  :: String -> NetType -> [Bit] -> Out ()
outputBitVecLocal name bvt@(BitVec a dir b) o
  = do state <- get
       let p = ports state
       put (state {ports = Port name LocalOutput bvt o : p})

-------------------------------------------------------------------------------

declareType :: String -> NetType -> Out NetType
declareType typeName typeDef
  = do netlist <- get
       put netlist{types = (typeName, typeDef) : types netlist}
       return (NamedType typeName)

-------------------------------------------------------------------------------

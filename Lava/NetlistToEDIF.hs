-------------------------------------------------------------------------------
--- $Id: NetlistToEDIF.hs#4 2010/10/12 20:13:55 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.NetlistToEDIF
where
import Control.Monad
import System.IO
import Data.Array

import Lava.CircuitGraphToVHDL
import Lava.ComputeNetDrivers
import Lava.Netlist
import Lava.PortRange
import Lava.RPM 
import Lava.Utils

-------------------------------------------------------------------------------

putXilinxEDIF :: Netlist -> IO ()
putXilinxEDIF netlist
  = do putStr ("Generating " ++ filename ++ " ...")
       file <- openFile filename WriteMode
       preamble file netlist
       mainPreamble file netlist
       mapM_ (putDriver file (types netlist) (netDrivers netlistWithDrivers) 
                        (ports netlist))
              [0..n-1]
       postamble file netlist
       hClose file
       putStrLn " [done]"
       putVHDLPackage file netlist
    where
    netlistWithDrivers = computeNetDrivers  netlist
    filename = circuitName netlist ++".edif"
    n = netCount netlist

-------------------------------------------------------------------------------
-- | 'putDriver' writes out the EDIF net driven by source net(i)

putDriver :: Handle -- ^ The file to write to
            -> [(String, NetType)]   -- ^ Type definitions
            ->  Array Int DrivenPorts  -- ^  An array of driven nets
            -> [PortDeclaration]  -- ^ Port declarations
            -> Int                -- ^ The net driver t
            -> IO () 
putDriver file typeDefs driven ports i | driven!i == [] = return ()
putDriver file typeDefs driven ports i
  = do hPutStrLn file ("            (net (rename net_" ++ show i ++ "__ \"net<" ++ show i ++ ">\")")
       hPutStrLn file "              (joined"
       sequence_ [hPutStrLn file ("                (portRef " ++ port ++ " (instanceRef " ++ inst ++ "))") | (port, inst) <- driven!i]
       putPortWires file typeDefs i ports
       hPutStrLn file "              )"
       hPutStrLn file "            )" 

-------------------------------------------------------------------------------

preamble :: Handle -> Netlist -> IO ()
preamble file netlist
  = do hPutStrLn file ("(edif " ++ name)
       hPutStrLn file "  (edifVersion 2 0 0)"
       hPutStrLn file "  (edifLevel 0)"
       hPutStrLn file "  (keywordMap (keywordLevel 0))"
       hPutStrLn file "  (external UNISIMS"
       hPutStrLn file "    (edifLevel 0)"
       hPutStrLn file "    (technology (numberDefinition))"
       -- declareBUF file
       declareComponents file [] (instances netlist)
       hPutStrLn file "  )"
   where
   name = circuitName netlist

-------------------------------------------------------------------------------

postamble :: Handle -> Netlist -> IO ()
postamble file netlist
  = do hPutStrLn file "          )"
       hPutStrLn file "      )"
       hPutStrLn file "    )"
       hPutStrLn file "  )"
       hPutStrLn file ""
       hPutStrLn file ("  (design " ++ name)
       hPutStrLn file ("    (cellRef " ++ name)
       hPutStrLn file ("      (libraryRef " ++ name ++ "_lib)")
       hPutStrLn file "    )"
       hPutStrLn file "  )"
       hPutStrLn file ")"
    where
    name = circuitName netlist

-------------------------------------------------------------------------------

declareComponents :: Handle -> [String] -> [Instance] -> IO ()
declareComponents _ _ [] = return ()
declareComponents file alreadyDeclared (inst:insts)
  = if (componentName inst `elem` alreadyDeclared) then
      declareComponents file alreadyDeclared insts
     else
       do declareComponent file inst
          declareComponents file (componentName inst : alreadyDeclared) insts

-------------------------------------------------------------------------------


declareComponent :: Handle -> Instance -> IO ()
declareComponent file inst
  = case component inst of
      Lut1 _ _ _ _ -> declareCell file "lut1" ["i0"] ["o"]
      Lut2 _ _ _ _ _ -> declareCell file "lut2" ["i0", "i1"] ["o"]
      Lut3 _ _ _ _ _ _ -> declareCell file "lut3" ["i0", "i1", "i2"] ["o"]
      Lut4 _ _ _ _ _ _ _ -> declareCell file "lut4"
                                       ["i0", "i1", "i2", "i3"] ["o"]
      Lut5 _ _ _ _ _ _ _ _ -> declareCell file "lut4"
                                    ["i0", "i1", "i2", "i3", "i4"] ["o"]
      Lut6 _ _ _ _ _ _ _ _ _ -> declareCell file "lut4"
                                    ["i0", "i1", "i2", "i3", "i4", "i5"] ["o"]
      PrimitiveGate inputs outputs -> declareCell file (componentName inst)
                                        (map fst inputs) (map fst outputs)
      other -> error ("declareComponent: " ++ show other)
      
-------------------------------------------------------------------------------

declareCell :: Handle -> String -> [String] -> [String] -> IO ()
declareCell file cellName inputs outputs
  = do hPutStrLn file ("    (cell " ++ cellName)
       hPutStrLn file  "      (cellType GENERIC)"
       hPutStrLn file  "        (view view_1"
       hPutStrLn file  "          (viewType NETLIST)"
       hPutStrLn file  "          (interface"
       sequence_ [hPutStrLn file ("            (port " ++ i ++ " (direction INPUT))") | i <- inputs]
       sequence_ [hPutStrLn file ("            (port " ++ o ++ " (direction OUTPUT))") | o <- outputs]
       hPutStrLn file  "          )"
       hPutStrLn file  "        )"
       hPutStrLn file  "    )"

-------------------------------------------------------------------------------

mainPreamble :: Handle -> Netlist -> IO ()
mainPreamble file netlist
  = do hPutStrLn file ("  (library " ++ name ++ "_lib")
       hPutStrLn file  "    (edifLevel 0)"
       hPutStrLn file  "    (technology (numberDefinition))"
       hPutStrLn file ("    (cell " ++ name)
       hPutStrLn file  "     (cellType GENERIC)"
       hPutStrLn file  "        (view view_1"
       hPutStrLn file  "          (viewType NETLIST)"
       hPutStrLn file  "          (interface"
       mapM_ (putEDIFPort file (types netlist)) (ports netlist)
       when (rlocOrigin netlist /= Nothing) $
         hPutStrLn file ("            (property RLOC_ORIGIN (string \"" ++
           showRLOC (xilinxArchitecture netlist) (x, y) ++ "\"))")
       hPutStrLn file "          )"
       hPutStrLn file "          (contents"
       -- declareBUFInstances file (ports netlist)
       mapM_ (putEDIFInstance file netlist) (instances netlist)
    where
    name = circuitName netlist
    Just (x, y) = rlocOrigin netlist

-------------------------------------------------------------------------------

putEDIFPort :: Handle -> [(String, NetType)] -> PortDeclaration -> IO ()
putEDIFPort file _ (Port name InputPort BitType _)
  = hPutStrLn file ("            (port " ++ name ++ " (direction INPUT))")
putEDIFPort file _ (Port name InputPort (BitVec a dir b) _)
  = hPutStrLn file ("            (port (array (rename " ++ name ++ " \"" ++
                     name ++ "<" ++ show a ++ ":" ++ show b ++ ">\") " ++
                     show (abs (a-b)+ 1) ++ ") (direction INPUT))")
putEDIFPort file _ (Port name InputPort 
                     (ArrayType al dir1 ah (BitVec bl dir2 bh)) nets)
  = sequence_ [putArrayOfArrayPort file name "INPUT" a b |
                a <- portOrder al dir1 ah, b <- portOrder bl dir2 bh]

putEDIFPort file _ (Port name OutputPort BitType _)
  = hPutStrLn file ("            (port " ++ name ++ " (direction OUTPUT))")
putEDIFPort file _ (Port name OutputPort (BitVec a dir b) _)
  = hPutStrLn file ("            (port (array (rename " ++ name ++ " \"" ++
                     name ++ "<" ++ show a ++ ":" ++ show b ++ ">\") " ++
                     show (abs (a-b)+ 1) ++ ") (direction OUTPUT))")

putEDIFPort file typeDefs (Port name dir (NamedType typeName) nets)
  = putEDIFPort file typeDefs (Port name dir def nets)
    where
    Just def = lookup typeName typeDefs

putEDIFPort file _ (Port name OutputPort 
                     (ArrayType al dir1 ah (BitVec bl dir2 bh)) nets)
  = sequence_ [putArrayOfArrayPort file name "OUTPUT" a b |
                 a <- portOrder al dir1 ah, b <- portOrder bl dir2 bh]


-------------------------------------------------------------------------------

putArrayOfArrayPort :: Handle -> String -> String -> Int -> Int -> IO ()
putArrayOfArrayPort file name dirtext a b
  = do hPutStrLn file ("            (port (rename " ++ name ++ "_" ++
                       show a ++ "__" ++ show b ++ "__ \"" ++
                       name ++ "<" ++ show a ++ "><" ++ show b ++ ">\") " ++
                       "(direction " ++ dirtext ++ "))")

-------------------------------------------------------------------------------


putEDIFInstance :: Handle -> Netlist -> Instance -> IO ()
putEDIFInstance file netlist inst
  = case component inst of
      Lut1 prog i0 o comment -> putLUTInstance file netlist inst prog
      Lut2 prog i0 i1 o comment -> putLUTInstance file netlist inst prog
      Lut3 prog i0 i1 i2 o comment -> putLUTInstance file netlist inst prog
      Lut4 prog i0 i1 i2 i3 o comment -> putLUTInstance file netlist inst prog
      Lut5 prog i0 i1 i2 i3 i4 o comment 
        -> putLUTInstance file netlist inst prog
      Lut6 prog i0 i1 i2 i3 i4 i5 o comment 
        -> putLUTInstance file netlist inst prog
      PrimitiveGate _ _ -> putPrimitiveInstance file netlist inst
    where
    instName = show (instanceNumber inst)

-------------------------------------------------------------------------------

putLUTInstance :: Handle -> Netlist -> Instance -> [Int] -> IO ()
putLUTInstance file netlist inst prog
  = do hPutStrLn file ("            (instance " ++ instName)
       hPutStrLn file ("             (viewRef view_1 (cellRef " ++ cellName
                       ++ " (libraryRef UNISIMS)))")
       hPutStrLn file ("              (property INIT (string \"" ++
                       edifLUTInit prog ++ "\"))")
       when (position inst == Placed) $
         hPutStrLn file ("              (property RLOC (string \"" ++
                         showRLOC (xilinxArchitecture netlist)
                             (x!i,  y!i) ++ "\"))")
       hPutStrLn file "            )"
   where
   (x, y) = computedShifts netlist
   cellName = componentName inst
   i = instanceNumber inst
   instName = cellName ++ "_" ++ show i

-------------------------------------------------------------------------------

putPrimitiveInstance :: Handle -> Netlist -> Instance -> IO ()
putPrimitiveInstance file netlist inst
  = do hPutStrLn file ("            (instance " ++ instName)
       hPutStrLn file ("             (viewRef view_1 (cellRef " ++ cellName
                       ++ " (libraryRef UNISIMS)))")
       when (position inst == Placed) $
         hPutStrLn file ("              (property RLOC (string \"" ++
                        showRLOC (xilinxArchitecture netlist)
                             (x!i,  y!i) ++ "\"))")
       hPutStrLn file "            )"
   where
   (x, y) = computedShifts netlist
   cellName = componentName inst
   i = instanceNumber inst
   instName = cellName ++ "_" ++ show i

-------------------------------------------------------------------------------

edifLUTInit:: [Int] -> String
edifLUTInit xs = show (binaryListToInt xs)

-------------------------------------------------------------------------------

binaryListToInt :: [Int] -> Int
binaryListToInt xs = sum [x*p | (x, p) <- zip (reverse xs) powersOfTwo]

-------------------------------------------------------------------------------

powersOfTwo :: [Int]
powersOfTwo = [2^n | n <- [0..]]

-------------------------------------------------------------------------------

declareBUF :: Handle -> IO ()
declareBUF file
  = do hPutStrLn file "    (cell buf"
       hPutStrLn file "      (cellType GENERIC)"
       hPutStrLn file "        (view view_1"
       hPutStrLn file "          (viewType NETLIST)"
       hPutStrLn file "          (interface"
       hPutStrLn file "            (port i (direction INPUT))"
       hPutStrLn file "            (port o (direction OUTPUT))"
       hPutStrLn file "          )"
       hPutStrLn file "        )"
       hPutStrLn file "    )"

-------------------------------------------------------------------------------

declareBUFInstances :: Handle -> [PortDeclaration] -> IO ()
declareBUFInstances file ports
  = mapM_ (declareBUFInstance file) ports

-------------------------------------------------------------------------------

declareBUFInstance :: Handle -> PortDeclaration -> IO ()
declareBUFInstance file (Port name _ _ _)
  = do hPutStrLn file ("            (instance buf_" ++ name)
       hPutStrLn file  "             (viewRef view_1 (cellRef buf (libraryRef UNISIMS))))"

-------------------------------------------------------------------------------

putPortWires :: Handle -> [(String, NetType)] -> Int -> 
                [PortDeclaration] -> IO ()
putPortWires file typeDefs i ports = mapM_ (putPortWire file typeDefs i) ports

-------------------------------------------------------------------------------

putPortWire :: Handle -> [(String, NetType)] -> Int -> PortDeclaration -> IO ()

putPortWire file typeDefs i (Port name _ BitType connections)
  = when (i `elem` connections) $
      hPutStrLn file ("                (portRef " ++ name ++ ")")

putPortWire file typeDefs  i (Port name _ (BitVec a dir b) connections)
  = sequence_ -- Multiple outputs may be driven so consider every match
        [hPutStrLn file ("                (portRef (member " ++ name ++
                      " " ++ show idx ++ "))") | idx <- idxs]
    where
    idxs = findConnection i connections (portRange a dir b)

putPortWire file typeDefs i 
            (Port name _ (ArrayType al dir1 ah (BitVec bl dir2 bh))
                    connections)
  = sequence_ -- Multiple outputs may be driven so consider every match
        [hPutStrLn file ("                (portRef (" ++ name ++
                      "_ " ++ show a ++ "__" ++ show idx ++ "__)") | 
                     a <- portRange al dir1 ah,
                     (a,idx) <- findConnection2D i connections' 
                                (portRange al dir1 ah) (portRange bl dir2 bh)]
    where
    bSize = (abs bh - bl) + 1
    connections' = chopList bSize connections

putPortWire file typeDefs i (Port name dir (NamedType typeName) cx)
  = putPortWire file typeDefs i (Port name dir def cx)
    where
    Just def = lookup typeName typeDefs

-------------------------------------------------------------------------------
-- | 'findConnection' takes a net number and a list of connectiosn
--   and returns the port indices which are connected to this Lava net.

findConnection :: Int -> [Int] -> [Int] -> [Int]
findConnection i connections prange
  = [idx | (idx, c) <- zip prange connections, c == i]

-------------------------------------------------------------------------------
-- | 'findConnection2D' takes a net number and a list of connectiosn
--   and returns the 2D port indices which are connected to this Lava net.

findConnection2D :: Int -> [[Int]] -> [Int] -> [Int] -> [(Int, Int)]
findConnection2D i connections prange1 prange2 
  = error (show i++ " " ++ show connections ++ " " ++ show prange1 ++ " " ++ show prange2)
findConnection2D i connections prange1 prange2
  =  concat [[(a, c) | c <- findConnection i bs prange2]
             | (a, bs) <- zip prange1 connections]

-------------------------------------------------------------------------------

putVHDLPackage :: Handle -> Netlist -> IO ()
putVHDLPackage file netlist
  = do putStr ("Generating " ++ filename ++ " ...")
       file <- openFile filename WriteMode
       vhdlPackage file (circuitName netlist) (ports netlist) (types netlist)
       hClose file
       putStrLn " [done]"
    where
    filename = circuitName netlist ++ "_package.vhd"

-------------------------------------------------------------------------------

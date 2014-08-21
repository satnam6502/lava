-------------------------------------------------------------------------------
--- $Id: CircuitGraphToVHDL.hs#20 2010/10/12 20:37:14 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.CircuitGraphToVHDL (putXilinxVHDL, vhdlPackage)
where
import System.IO
	
import Control.Monad.State
import Data.Array.Unboxed
import Lava.Netlist
import Lava.PortRange
import Lava.RPM
import Lava.Utils
import Lava.Version

-- * Generating VHDL from a Lava netlist

-------------------------------------------------------------------------------

putXilinxVHDL :: Netlist -> IO ()
putXilinxVHDL netlist
  = do putStr ("Generating " ++ filename ++ " ...")
       file <- openFile filename WriteMode
       ds <- dateString
       hPutStrLn file (replicate 79 '-')
       hPutStrLn file ("-- Automatically generated by Lava version " ++
                        versionStr)
       hPutStrLn file ("-- Generated on " ++ ds) 
       hPutStrLn file ("-- Architecture: " ++ show (xilinxArchitecture netlist))
       when (wh /= (0,0)) $
         hPutStrLn file ("-- Layout (width, height) = " ++ show wh)
       hPutStrLn file (replicate 79 '-')
       hPutStrLn file ""

       circuitToVHDL file netlist

       hClose file
       putStrLn " [done]"
    where
    name = circuitName netlist
    filename = name++".vhd"
    circuitPorts = ports netlist
    circuitInstances = instances netlist
    nC = netCount netlist
    (major1, major2, minor, patch) = lavaVersion
    versionStr = show major1 ++ "." ++ show major2 ++ "." ++ show minor ++ 
                 "." ++ show patch
    wh = if layout netlist == [] then
           (0, 0)
         else
           sizeOfLayout (head (layout netlist))

-------------------------------------------------------------------------------

circuitToVHDL :: Handle -> Netlist -> IO ()
circuitToVHDL file netlist
  = do mapM_ (circuitToVHDL file) (subCircuits netlist)
       when (wh /= (0,0)) $
         hPutStrLn file ("-- Layout (width, height) = " ++ show wh)
       vhdlPackage file name circuitPorts (types netlist)
       vhdlEntity file name circuitPorts (rlocOrigin netlist)
                       (xilinxArchitecture netlist)
       vhdlArchitecture file name (subCircuits netlist)
                        circuitPorts (types netlist) circuitInstances nC
                        (computedShifts netlist) (xilinxArchitecture netlist)
       hPutStrLn file ""
       hPutStrLn file (replicate 79 '-')
       hPutStrLn file ""
    where
    name = circuitName netlist
    filename = name++".vhd"
    circuitPorts = ports netlist
    circuitInstances = instances netlist
    nC = netCount netlist
    wh = if layout netlist == [] then
           (0, 0)
         else
           sizeOfLayout (head (layout netlist))

-------------------------------------------------------------------------------

vhdlPackage :: Handle -> String -> [PortDeclaration] -> [(String, NetType)] 
               -> IO ()
vhdlPackage handle name interface typs
  = do hPutStr handle (unlines entity)
    where
    entity =
     ["library ieee ;",
      "use ieee.std_logic_1164.all ;",
      "package " ++ name ++ "_package is"] ++
      map (typeDefToVHDL) (reverse typs) ++ 
     [
      "",
      "  component " ++ name ++ " is ",
      "    port(" ++ vhdlPorts (reverse interface)] ++
     ["        ) ;",
      "  end component " ++ name ++ " ;",
      "",
      "end package " ++ name ++ "_package ;",
      ""]
     
-------------------------------------------------------------------------------

typeDefToVHDL :: (String, NetType) -> String
typeDefToVHDL (name, BitType) 
  = "  subtype " ++ name ++ " is " ++ vhdlType BitType 
typeDefToVHDL (name, bvt@(BitVec a dir b))
  = "  type " ++ name ++ " is " ++ vhdlType bvt
typeDefToVHDL (name, atyp@(ArrayType a dir b t))
  = "  type " ++ name ++ " is " ++ vhdlType atyp
typeDefToVHDL other = error ("typeDefToVHDL: " ++ show other)

-------------------------------------------------------------------------------

vhdlType :: NetType -> String
vhdlType BitType = "std_logic" ;
vhdlType (BitVec a dir b) 
  = "std_logic_vector (" ++ show a ++ " " ++ showDir dir ++ " " ++ show b
     ++ ") ;"
vhdlType (ArrayType a dir b typ)
  = "array (" ++ show a ++ " " ++ showDir dir ++ " " ++ show b ++ ") of " ++
    vhdlType typ 
vhdlType (NamedType name) = name

-------------------------------------------------------------------------------

vhdlEntity :: Handle -> String -> [PortDeclaration] -> Maybe (Int, Int)
              -> XilinxArchitecture -> IO ()
vhdlEntity handle name interface maybeOrigin arch
  = do hPutStr handle (unlines entity)
    where
    entity =
     ["library ieee ;",
      "use ieee.std_logic_1164.all ;",
      "use work." ++ name ++ "_package.all ;",
      "entity " ++ name ++ " is ",
      "  port(" ++ vhdlPorts (reverse interface)] ++
     ["      ) ;"] ++
     (if maybeOrigin == Nothing then
        []
      else
        ["  attribute rloc_origin : string ;",
         "  attribute rloc_origin of " ++ name ++ " : entity is \"" ++
         showRLOC arch (x, y) ++ "\" ;" ]
     ) ++
     ["end entity " ++ name ++ " ;",
      ""]
    Just (x, y) = maybeOrigin
     
-------------------------------------------------------------------------------

vhdlPorts :: [PortDeclaration] -> String
vhdlPorts intf
  = insertString " ;\n       " (map vhdlPort [p | p <- intf, isNotKept p])

-------------------------------------------------------------------------------

vhdlPort :: PortDeclaration -> String

vhdlPort (Port name InputPort typ net)
  = "signal " ++ name ++ " : in " ++ showType typ

vhdlPort (Port name OutputPort typ net)
  = "signal " ++ name ++ " : out " ++ showType typ   

-------------------------------------------------------------------------------

isNotKept :: PortDeclaration -> Bool
isNotKept (Port _ LocalInput _ _) = False
isNotKept (Port _ LocalOutput _ _) = False
isNotKept other = True

-------------------------------------------------------------------------------

showType :: NetType -> String
showType BitType = "std_logic"
showType (BitVec low dir high)
   = "std_logic_vector (" ++ show low ++ " " ++ showDir dir ++ " " ++ show high ++
     ")"
showType (NamedType typeName) = typeName

-------------------------------------------------------------------------------

showDir :: Dir -> String
showDir To = "to"
showDir Downto = "downto"

-------------------------------------------------------------------------------

vhdlArchitecture :: Handle ->
                    String -> [Netlist] ->
                    [PortDeclaration] -> [(String, NetType)] ->
                    [Instance] -> Int -> 
                    (Array Int Int, Array Int Int) -> XilinxArchitecture ->
                    IO ()
vhdlArchitecture file name subcirs ports typeDefs 
                 gates netCount shifts architecture
  = do hPutStr file
        (unlines (["library ieee, unisim ;",
          "use ieee.std_logic_1164.all ;",
          "use unisim.vcomponents.all ;"] ++
         ["use work." ++ circuitName nl ++ "_package.all ;" | nl <- subcirs] ++
         ["architecture lava of " ++ name ++ " is ",
          "  attribute rloc : string ;",
          "  attribute keep : string ;",
          netDefs]))
       declareKeptPorts file ports
       writeRLOCs file architecture shifts gates
       hPutStr file "begin\n"
       -- hPutStrLn file "  net(0) <= '0' ;" 
       -- hPutStrLn file "  net(1) <= '1' ;"
       wirePorts file typeDefs  ports
       vhdlInstances file gates
       hPutStr file "end architecture lava ;\n"
     where
     netDefs :: String
     netDefs = if netCount == 1 then
                  "  -- no local Lava nets"
               else
                  "  signal net : std_logic_vector (0 to " ++ 
                  show (netCount-1) ++ ") ;"
-------------------------------------------------------------------------------

declareKeptPorts :: Handle -> [PortDeclaration] -> IO ()
declareKeptPorts file [] = return ()
declareKeptPorts file ((Port name LocalOutput typ nets):rest)
  = do hPutStrLn file ("  signal " ++ name ++ " :  " ++ showType typ ++ " ;")
       declareKeptPorts file rest
       hPutStrLn file ("  attribute keep of " ++ name ++ " : signal is \"true\" ;")
declareKeptPorts file ((Port name LocalInput typ nets):rest)
  = do hPutStrLn file ("  signal " ++ name ++ " :  " ++ showType typ ++ " ;")
       hPutStrLn file ("  attribute keep of " ++ name ++ " : signal is \"true\" ;")
       declareKeptPorts file rest
declareKeptPorts file (_:rest)
  =  declareKeptPorts file rest

-------------------------------------------------------------------------------

wirePorts :: Handle -> [(String, NetType)] -> [PortDeclaration] -> IO ()
wirePorts file typeDefs = mapM_ (wirePort file typeDefs)

-------------------------------------------------------------------------------

wirePort :: Handle -> [(String, NetType)] -> PortDeclaration -> IO ()

-- Wire up a single bit input port.
wirePort file _ (Port name InputPort BitType [net])
  = hPutStrLn file ("  net(" ++ show net ++ ") <= " ++ name ++ ";")

-- Wire up an input bit-vector
wirePort file _ (Port name InputPort (BitVec a dir b) nets)
  = sequence_ 
      [hPutStrLn file ("  net(" ++ show net ++ ") <= " ++ name ++ "(" ++ show i ++ ")" ++ ";") | (i, net) <- zip (portRange a dir b) nets]

-- Wire up an array of bit-vector input port
wirePort file _ (Port name InputPort (ArrayType al dir1 ah (BitVec bl dir bh)) nets)
  = sequence_
    [ hPutStrLn file ("  net(" ++ show net ++ ") <= " ++ name ++ "(" ++ show a ++ ")(" ++ show b ++ ");") 
      | ((a,b),net) <- zip [(a,b) |  a <- portRange al dir1 ah, b <- portRange bl dir bh] nets]

wirePort file _ (Port name LocalInput (ArrayType al dir1 ah (BitVec bl dir bh)) nets)
  = sequence_
    [ hPutStrLn file ("  net(" ++ show net ++ ") <= " ++ name ++ "(" ++ show a ++ ")(" ++ show b ++ ");") 
      | ((a,b),net) <- zip [(a,b) |  a <- portRange al dir1 ah, b <- portRange bl dir bh] nets]

-- Wire up a local input bit-vector
wirePort file _ (Port name LocalInput (BitVec a dir b) nets)
  = sequence_ 
      [hPutStrLn file ("  net(" ++ show net ++ ") <= " ++ name ++ "(" ++ show i ++ ")" ++ ";") | (i, net) <- zip (portRange a dir b) nets]


-- Wire up a single bit output port.
wirePort file _ (Port name OutputPort BitType [net])
  = hPutStrLn file ("  " ++ name ++ " <= net(" ++show  net ++ ") ;")

-- Wire up an output bit-vector
wirePort file _ (Port name OutputPort (BitVec a dir b) nets)
  = sequence_ [hPutStrLn file ("  " ++ name ++ "(" ++ show i ++ ") <= net(" ++show  net ++ ") ;")
               | (i, net) <- zip (portRange a dir b) nets]
wirePort file _ (Port name LocalOutput (BitVec a dir b) nets)
  = sequence_ [hPutStrLn file ("  " ++ name ++ "(" ++ show i ++ ") <= net(" ++show  net ++ ") ;")
               | (i, net) <- zip (portRange a dir b) nets]


-- Wire up an array of bit-vector output port
wirePort file _ (Port name OutputPort (ArrayType al dir1 ah (BitVec bl dir bh)) nets)
  = sequence_
    [hPutStrLn file ("  " ++ name ++ "(" ++ show a ++ ")(" ++ show b ++ ") <= " ++ "  net(" ++ show net ++ ") ; ")
      | ((a,b),net) <- zip [(a,b) |  a <- portRange al dir1 ah, b <- portRange bl dir bh] nets]

wirePort file _ (Port name LocalOutput (ArrayType al dir1 ah (BitVec bl dir bh)) nets)
  = sequence_
    [hPutStrLn file ("  " ++ name ++ "(" ++ show a ++ ")(" ++ show b ++ ") <= " ++ "  net(" ++ show net ++ ") ; ")
      | ((a,b),net) <- zip [(a,b) |  a <- portRange al dir1 ah, b <- portRange bl dir bh] nets]

-- Wire up named type ports

wirePort file typeDefs (Port name polarity (NamedType typeName) nets)
  = wirePort file typeDefs (Port name polarity def nets)
    where
    Just def = lookup typeName typeDefs

-------------------------------------------------------------------------------

vhdlInstances :: Handle -> [Instance] -> IO ()
vhdlInstances file gates
   = mapM_ (vhdlInstance file) gates

-------------------------------------------------------------------------------
   
vhdlInstance :: Handle -> Instance -> IO ()
vhdlInstance file inst
  = case component inst of
      Lut1 opBits i0 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut1 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), o => net(" ++ 
                     show o ++ ")) " ++
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut2 opBits i0 i1 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut2 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "o => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut2_l opBits i0 i1 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut2_l " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "lo => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut3 opBits i0 i1 i2 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut3 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "i2 => net(" ++ show i2 ++ "), " ++
                               "o => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut4 opBits i0 i1 i2 i3 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut4 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "i2 => net(" ++ show i2 ++ "), " ++
                               "i3 => net(" ++ show i3 ++ "), " ++
                               "o => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut5 opBits i0 i1 i2 i3 i4 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut5 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "i2 => net(" ++ show i2 ++ "), " ++
                               "i3 => net(" ++ show i3 ++ "), " ++
                               "i4 => net(" ++ show i4 ++ "), " ++
                               "o => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      Lut6 opBits i0 i1 i2 i3 i4 i5 o comment ->
         hPutStrLn file ("  " ++ instName ++ " : lut6 " ++
                     "generic map (init => \"" ++ showOpBits opBits ++
                      "\") " ++
                     "port map (i0 => net(" ++ show i0 ++ "), " ++
                               "i1 => net(" ++ show i1 ++ "), " ++
                               "i2 => net(" ++ show i2 ++ "), " ++
                               "i3 => net(" ++ show i3 ++ "), " ++
                               "i4 => net(" ++ show i4 ++ "), " ++
                               "i5 => net(" ++ show i5 ++ "), " ++
                               "o => net(" ++ show o ++ ")) " ++ 
                     if comment /= "" then "; -- " ++ comment else ";")
      PrimitiveGate inputPorts outputs ->
         hPutStrLn file ("  " ++ instName ++ " : " ++ componentName inst ++
                         " port map (" ++
                   insertCommas [p ++ " => net(" ++ show a ++ ")" | (p,a) <- inputPorts ++ outputs] ++
                  ") ; ")
    where
    instName = componentName inst ++ "_" ++ show (instanceNumber inst)

-------------------------------------------------------------------------------

showOpBits = map intToChar 
intToChar 0 = '0'
intToChar 1 = '1'

-------------------------------------------------------------------------------

writeRLOCs :: Handle -> XilinxArchitecture -> (Array Int Int, Array Int Int) 
              -> [Instance] -> IO ()
writeRLOCs file arch dxy  = mapM_ (writeRLOC file arch dxy) 

-------------------------------------------------------------------------------

writeRLOC :: Handle -> XilinxArchitecture ->  (Array Int Int, Array Int Int) 
             -> Instance -> IO ()
writeRLOC file arch (dx, dy) inst
   = if pos == Unplaced then
       return ()
     else
       hPutStrLn file ("  attribute rloc of " ++ instName ++ " : label is \"" 
                         ++ showRLOC arch (dx!i,dy!i) ++ "\" ;")
     where
     pos = position inst
     i = instanceNumber inst
     instName = componentName inst ++ "_" ++ show i

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--- $Id: Lava.hs#17 2010/10/12 20:13:55 REDMOND\\satnams $
-------------------------------------------------------------------------------

-- | Xilinx Lava is a library for FPGA circuit design with a focus
--   on circuit layout.

module Lava (module Lava.Combinators,
             module Lava.Ports,
             Dir(..), NetType(..), Netlist, Out, Bit, XilinxArchitecture(..),
             setOrigin,
             module Lava.Col,
             module Lava.ComputeNetlist,
             module Lava.CircuitGraphToVHDL,
             module Lava.OverlayTile,
             module Lava.Middle,
	     module Lava.PrimitiveGates,
             module Lava.Instantiate,
             module Lava.NetlistToEDIF,
             module Lava.LUTGates,
             module Lava.Version,
             one, zero)
where

import Lava.CircuitGraphToVHDL (putXilinxVHDL)
import Lava.Col
import Lava.Combinators
import Lava.ComputeNetlist (computeNetlist)
import Lava.Netlist (Dir(..), NetType(..), Netlist, Out, Bit, 
                     XilinxArchitecture(..), setOrigin)
import Lava.OverlayTile
import Lava.Middle
import Lava.Ports
import Lava.PrimitiveGates
import Lava.Instantiate
import Lava.NetlistToEDIF
import Lava.LUTGates
import Lava.Version

one :: Bit
one = 1

zero :: Bit
zero = 0

-------------------------------------------------------------------------------


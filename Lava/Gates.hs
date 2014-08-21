-------------------------------------------------------------------------------
--- $Id: Gates.hs#14 2010/10/06 10:24:29 REDMOND\\satnams $
-------------------------------------------------------------------------------

-- | This Lava.Gates module provides a collection of Xilinx low-level
--   components.

module Lava.Gates (module Lava.Gates)
where
import Lava.Combinators
import Lava.Netlist
import Lava.LUTGates
import Lava.PrimitiveGates

-- * Lava Gates
             
-- ** LUT-based gates

-------------------------------------------------------------------------------
--- LUT-based gates
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | The 'inv' function implements an invertor explicitly with a LUT1.

inv :: Bit -- ^ The input i0
       -> Out Bit -- ^ The output o
inv = lut1gate not "inv"

-------------------------------------------------------------------------------
-- | The 'and2' function implements an AND gate explicitly with a LUT2.

and2 :: (Bit, Bit)  -- ^ inputs (i0, i1)
        -> Out Bit  -- ^ output o
and2 = lut2gate (&&) "and2"

-------------------------------------------------------------------------------
-- | The 'and3' function implements an AND gate explicitly with a LUT3.

and3 :: (Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2)
        -> Out Bit       -- ^ output o
and3 = lut3gate (\i0 i1 i2 -> i0 && i1 && i2) "and3"

-------------------------------------------------------------------------------
-- | The 'and4' function implements an AND gate explicitly with a LUT4.

and4 :: (Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3)
        -> Out Bit            -- ^ output o
and4 = lut4gate (\i0 i1 i2 i3 -> i0 && i1 && i2 && i3) "and4"

-------------------------------------------------------------------------------
-- | The 'and5' function implements an AND gate explicitly with a LUT5.

and5 :: (Bit, Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3, i4)
        -> Out Bit                  -- ^ output o
and5 = lut5gate (\i0 i1 i2 i3 i4 -> i0 && i1 && i2 && i3 && i4) "and5"

-------------------------------------------------------------------------------
-- | The 'and6' function implements an AND gate explicitly with a LUT6.

and6 :: (Bit, Bit, Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3, i4, i5)
        -> Out Bit                  -- ^ output o
and6 = lut6gate (\i0 i1 i2 i3 i4 i5 -> i0 && i1 && i2 && i3 && i4 && i5) "and6"

-------------------------------------------------------------------------------
-- | The 'or2' function implements an OR gate explicitly with a LUT2. 

or2 :: (Bit,  Bit) -- ^ inputs (i0, i1)
        -> Out Bit -- ^ output o
or2 = lut2gate (||) "or"

-------------------------------------------------------------------------------
-- | The 'or3' function implements an AND gate explicitly with a LUT3.

or3 :: (Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2)
        -> Out Bit       -- ^ output o
or3 = lut3gate (\i0 i1 i2 -> i0 || i1 || i2) "or3"

-------------------------------------------------------------------------------
-- | The 'or4' function implements an AND gate explicitly with a LUT4.

or4 :: (Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3)
        -> Out Bit            -- ^ output o
or4 = lut4gate (\i0 i1 i2 i3 -> i0 || i1 || i2 || i3) "or4"

-------------------------------------------------------------------------------
-- | The 'or5' function implements an AND gate explicitly with a LUT5.

or5 :: (Bit, Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3, i4)
        -> Out Bit                  -- ^ output o
or5 = lut5gate (\i0 i1 i2 i3 i4 -> i0 || i1 || i2 || i3 || i4) "or5"

-------------------------------------------------------------------------------
-- | The 'and6' function implements an AND gate explicitly with a LUT6.

or6 :: (Bit, Bit, Bit, Bit, Bit, Bit)  -- ^ inputs (i0, i1, i2, i3, i4, i5)
        -> Out Bit                  -- ^ output o
or6 = lut6gate (\i0 i1 i2 i3 i4 i5 -> i0 || i1 || i2 || i3 || i4 || i5) "or6"

-------------------------------------------------------------------------------
-- | The 'nor2' function implements an NOR gate explicitly with a LUT2. 

nor2 :: (Bit,  Bit) -- ^ inputs (i0, i1)
        -> Out Bit  -- ^ output o
nor2 = lut2gate (\i0 i1 -> not (i0 || i1)) "nor"

-------------------------------------------------------------------------------
-- | The 'xor2' function implements an XOR gate explicitly with a LUT2.

xor2 :: (Bit,  Bit) -- ^ inputs (i0, i1)
        -> Out Bit  -- ^ output o
xor2 = lut2gate (/=) "xor"

-------------------------------------------------------------------------------
-- | The 'xnor2' function implements an XOR gate explicitly with a LUT2.

xnor2 :: (Bit,  Bit) -- ^ inputs (i0, i1)
          -> Out Bit -- ^ output o
xnor2 = lut2gate (==) "nxor"

-------------------------------------------------------------------------------
-- | A multiplexor implemented with a LUT3

mux :: (Bit, (Bit,  Bit)) -- ^ inputs (s, (i0, i1))
       -> Out Bit -- ^ output o
mux (s, (i0, i1)) = lut3gate (\s i0 i1 -> if s then i1 else i0) "mux" 
                             (s, i0, i1)

-------------------------------------------------------------------------------

-- ** Carry-chain elements

-------------------------------------------------------------------------------

muxcy :: (Bit, (Bit, Bit)) -- ^ (s, (di, ci))
         -> Out Bit        -- ^ o
muxcy (s, (di, ci))
  = do [o] <- primitiveGate  "muxcy" [("ci",ci), ("di", di), ("s", s)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

muxcy_d :: Bit                -- ^ d
           -> Bit             -- ^ ci 
           -> Bit             -- ^ di
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxcy_d ci di s 
  = do [o, lo] <- primitiveGate "muxcy_d" [("ci",ci), ("di", di), ("s", s)] 
                                ["o", "lo"] (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxcy_l :: Bit                -- ^ d
           -> Bit             -- ^ ci 
           -> Bit             -- ^ di
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxcy_l ci di s 
  = do [o, lo] <- primitiveGate "muxcy_l" [("ci",ci), ("di", di), ("s", s)] 
                                ["o", "lo"] (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf5 :: Bit    -- ^ i0
         -> Bit -- ^ i1
         -> Bit -- ^ s
         -> Out Bit -- ^o
muxf5 i0 i1 s
  = do [o] <- primitiveGate  "muxf5" [("i0",i0), ("i1", i1), ("s", s)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

muxf5_d :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf5_d i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf5_d" [("i0",i0), ("i1", i1), ("s", s)] 
                                 ["o"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf5_l :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf5_l i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf5_l" [("i0",i0), ("i1", i1), ("s", s)] 
                                           ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf6 :: Bit    -- ^ i0
         -> Bit -- ^ i1
         -> Bit -- ^ s
         -> Out Bit -- ^o
muxf6 i0 i1 s
  = do [o] <- primitiveGate  "muxf6" [("i0",i0), ("i1", i1), ("s", s)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

muxf6_d :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf6_d i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf6_d" [("i0",i0), ("i1", i1), ("s", s)] 
                                 ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf6_l :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf6_l i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf6_l" [("i0",i0), ("i1", i1), ("s", s)] 
                                           ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf7 :: Bit    -- ^ i0
         -> Bit -- ^ i1
         -> Bit -- ^ s
         -> Out Bit -- ^o
muxf7 i0 i1 s
  = do [o] <- primitiveGate  "muxf7" [("i0",i0), ("i1", i1), ("s", s)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

muxf7_d :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf7_d i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf7_d" [("i0",i0), ("i1", i1), ("s", s)] 
                                 ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf7_l :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf7_l i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf7_l" [("i0",i0), ("i1", i1), ("s", s)] 
                                           ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf8 :: Bit    -- ^ i0
         -> Bit -- ^ i1
         -> Bit -- ^ s
         -> Out Bit -- ^o
muxf8 i0 i1 s
  = do [o] <- primitiveGate  "muxf8" [("i0",i0), ("i1", i1), ("s", s)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

muxf8_d :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf8_d i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf8_d" [("i0",i0), ("i1", i1), ("s", s)] 
                                 ["o"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

muxf8_l :: Bit                -- ^ i0
           -> Bit             -- ^ i1 
           -> Bit             -- ^ i2
           -> Out (Bit, Bit)  -- ^ (o, lo)
muxf8_l i0 i1 s
  = do [o, lo] <- primitiveGate  "muxf8_l" [("i0",i0), ("i1", i1), ("s", s)] 
                                           ["o", "lo"]  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

xorcy :: (Bit, Bit)        -- ^ (li, ci)
         -> Out Bit        -- ^ o
xorcy (li, ci) 
  = do [o] <- primitiveGate  "xorcy" [("ci",ci), ("li", li)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

xorcy_d :: Bit               -- ^ ci
           -> Bit            -- ^ di
           -> Out (Bit, Bit) -- ^ (ci, li)
xorcy_d ci li 
  = do [o, lo] <- primitiveGate  "xorcy_d" [("ci",ci), ("li", li)] ["o"] 
                  (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

xorcy_l :: Bit               -- ^ ci
           -> Bit            -- ^ di
           -> Out (Bit, Bit) -- ^ (ci, li)
xorcy_l ci li 
  = do [o, lo] <- primitiveGate  "xorcy_l" [("ci",ci), ("li", li)] ["o", "lo"] 
                                 (Just (1,1))
       return (o, lo)

-------------------------------------------------------------------------------

-- ** Flip-flops

-------------------------------------------------------------------------------

fd :: Bit        -- ^ clk
      -> Bit     -- ^ i
      -> Out Bit -- ^ q
fd clk i 
  = do [q] <- primitiveGate  "fd" [("c",clk), ("d", i)] ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdc :: Bit        -- ^ clk
       -> Bit     -- clr 
       -> Bit     -- ^i
       -> Out Bit -- ^ q
fdc clk clr i 
  = do [q] <- primitiveGate  "fdc" [("c",clk), ("clr", clr), ("d", i)] 
                             ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdc_1 :: Bit        -- ^ clk
         -> Bit     -- ^ clr
         -> Bit     -- ^ i
         -> Out Bit -- ^ q
fdc_1 clk clr i 
  = do [q] <- primitiveGate  "fdc_1" [("c",clk), ("clr", clr), ("d", i)] 
                             ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdce :: Bit        -- ^ clk 
        -> Bit     -- ^ ce
        -> Bit     -- ^ clr
        -> Bit     -- ^ d
        -> Out Bit -- ^ q
fdce clk ce clr d
  = do [q] <- primitiveGate  "fdce" [("c",clk), ("clr", clr), ("d", d), 
                             ("ce", ce)]  ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdce_1 :: Bit        -- ^ clk
          -> Bit     -- ^ ce
          -> Bit     -- ^ clr
          -> Bit     -- ^ d
          -> Out Bit -- ^ q
fdce_1 clk ce clr d
  = do [q] <- primitiveGate  "fdce_1" [("c",clk), ("clr", clr), ("d", d), 
                             ("ce", ce)]  ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdcp :: Bit -- ^ clk
        -> Bit -- ^ clr
        -> Bit  -- ^ pre
        -> Bit -- ^ d
        -> Out Bit -- ^ q
fdcp clk clr pre d
  = do [q] <- primitiveGate  "fdcp" [("c",clk), ("clr", clr), ("d", d), 
                             ("pre", pre)]  ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdcpe :: Bit -- ^ clk
         -> Bit     -- ^ ce
         -> Bit     -- ^ clr
         -> Bit     -- ^ pre
         -> Bit     -- ^ d
         -> Out Bit -- ^ q
fdcpe clk ce clr pre d
  = do [q] <- primitiveGate  "fdcpe" [("c",clk), ("clr", clr), ("d", d), 
                             ("pre", pre), ("ce",ce)]  ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

fdcpe_1 :: Bit -- ^ clk
         -> Bit     -- ^ ce
         -> Bit     -- ^ clr
         -> Bit     -- ^ pre
         -> Bit     -- ^ d
         -> Out Bit -- ^ q
fdcpe_1 clk ce clr pre d
  = do [q] <- primitiveGate  "fdcpe_1" [("c",clk), ("clr", clr), ("d", d), 
                             ("pre", pre), ("ce",ce)]  ["q"] (Just (1,1))
       return q

-------------------------------------------------------------------------------

-- ** Shift-register primitives

-------------------------------------------------------------------------------
-- | 16-bit shift register look-up table with clock enable

srl16e :: Bit        -- ^ d 
          -> Bit     -- ^ clk
          -> Bit     -- ^ ce
          -> Bit     -- ^ a0
          -> Bit     -- ^ a1
          -> Bit     -- ^ a2
          -> Bit     -- ^ a3
          -> Out Bit -- ^ q
srl16e d clk ce a0 a1 a2 a3
  = do [q] <- primitiveGate "srl16e" [("c",clk), ("ce",ce), ("a0",a0),
                                      ("a1",a1), ("a2",a2), ("a3",a3)]
                            ["q"] Nothing
       return q

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- ** Gates implemented in place of a slice latch

-------------------------------------------------------------------------------
-- |  Two input and gate implemented in place of a slice latch

and2b1l :: Bit        -- ^ di
           -> Bit     -- ^ sri
           -> Out Bit -- ^ o
and2b1l di sri
  = do [o] <- primitiveGate  "and2b1l" [("di",di), ("sri", sri)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------
-- |  Two input and gate implemented in place of a slice latch

or2l :: Bit        -- ^ di
        -> Bit     -- ^ sri
        -> Out Bit -- ^ o
or2l di sri
  = do [o] <- primitiveGate  "or2l" [("di",di), ("sri", sri)] ["o"] 
              (Just (1,1))
       return o

-------------------------------------------------------------------------------

-- ** Buffers

-------------------------------------------------------------------------------
--- BUFFERS
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Dedicated input clock buffer 

ibufg :: Bit        -- ^ i
         -> Out Bit -- ^ o
ibufg i
  = do [o] <- primitiveGate  "ibufg" [("i", i)] ["o"] Nothing
       return o

-------------------------------------------------------------------------------
-- | Global clock buffer

bufg :: Bit        -- ^ i
         -> Out Bit -- ^ o
bufg i
  = do [o] <- primitiveGate  "bufg" [("i", i)] ["o"] Nothing
       return o

-------------------------------------------------------------------------------
-- | Global clock buffer

bufgp :: Bit        -- ^ i
         -> Out Bit -- ^ o
bufgp i
  = do [o] <- primitiveGate  "bufgp" [("i", i)] ["o"] Nothing
       return o

-------------------------------------------------------------------------------
-- | Output buffer

obufg :: Bit        -- ^ i
         -> Out Bit -- ^ o
obufg i
  = do [o] <- primitiveGate  "obuf" [("i", i)] ["o"] Nothing
       return o

-------------------------------------------------------------------------------

-- ** Double data rate (DDR) components

-------------------------------------------------------------------------------
-- | Output buffer

obufds :: Bit              -- ^ i
         -> Out (Bit, Bit) -- ^ (o, ob)
obufds i
  = do [o, ob] <- primitiveGate  "bufds" [("i", i)] ["o", "ob"] Nothing
       return (o, ob)

-------------------------------------------------------------------------------

-- * Muxing of buses

-------------------------------------------------------------------------------

muxBit :: Bit -> (Bit, Bit) -> Out Bit
muxBit sel (d0, d1) = lut3gate muxFn "muxBit" (sel, d0, d1)

-------------------------------------------------------------------------------

muxBit' :: Bit -> (Bit, Bit) -> Out Bit
muxBit' sel (d0, d1) = lut3gate muxFn' "muxBit'" (sel, d0, d1)

-------------------------------------------------------------------------------
-- The behaviour of a multiplexor.

muxFn :: Bool -> Bool -> Bool -> Bool
muxFn sel d0 d1
  = if sel then
      d1
    else
      d0

-------------------------------------------------------------------------------
-- A multiplexor which has an inverted control input w.r.t. muxBit

muxFn' :: Bool -> Bool -> Bool -> Bool
muxFn' sel d0 d1
  = if sel then
      d0
    else
      d1
-------------------------------------------------------------------------------

muxBus :: (Bit, ([Bit], [Bit])) -> Out [Bit]
muxBus (sel, (a,b)) = (ziP >-> maP (muxBit sel)) (a, b)

-------------------------------------------------------------------------------

muxBus' :: (Bit, ([Bit], [Bit])) -> Out [Bit]
muxBus' (sel, (a,b)) = (ziP >->  maP (muxBit' sel)) (a, b)

-------------------------------------------------------------------------------

vreg :: Bit -> [Bit] -> Out [Bit]
vreg clk = maP (fd clk)

-------------------------------------------------------------------------------

-- ** Power and ground

-------------------------------------------------------------------------------
-- | A supply of a constant zero signal.

gnd :: Out Bit
gnd = do [g] <- primitiveGate  "gnd" [] ["g"] Nothing
         return g

-------------------------------------------------------------------------------
-- | A supply of a constant one signal.

vcc :: Out Bit
vcc = do [p] <- primitiveGate  "vcc" [] ["p"] Nothing
         return p

-------------------------------------------------------------------------------

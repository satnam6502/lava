-------------------------------------------------------------------------------
--- $Id: Components.hs#4 2010/09/29 15:05:17 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Lava.Components
where

-------------------------------------------------------------------------------

data Component
  = Lut1   [Int] Int Int String
  | Lut2   [Int] Int Int Int String
  | Lut2_l [Int] Int Int Int String
  | Lut3   [Int] Int Int Int Int String
  | Lut4   [Int] Int Int Int Int Int String
  | Lut5   [Int] Int Int Int Int Int Int String
  | Lut6   [Int] Int Int Int Int Int Int Int String
  | PrimitiveGate [(String, Int)] [(String, Int)]
    deriving (Eq, Show)

-------------------------------------------------------------------------------

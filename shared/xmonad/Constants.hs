module Constants where

import           XMonad (Dimension)

--[ Mathematical constants ]--

φ :: Double
φ = phi

phi :: Double
phi = (1 + sqrt 5) / 2.0

-- | Colors
colorMain, colorSecondary   ::  String
colorMain                   =   "#268BD2"
colorSecondary              =   "#657b83"

-- | Terminal emulator
myTerminal                  ::  String
myTerminal                  =   "urxvt"

-- | The with of the borders between windows
myBorderWidth               ::  Dimension
myBorderWidth               =   1

-- | Space between tiles
tileSpacing                 :: Int
tileSpacing                 = 3


module Internet where

import           XMonad (X, spawn)

internet :: X ()
internet = spawn "google-chrome --disable-gpu"


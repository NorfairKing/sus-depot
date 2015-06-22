module Layout where

import           XMonad                   (Full (..), Mirror (..), Tall (..),
                                           (|||))
import           XMonad.Hooks.ManageDocks (avoidStruts)
import           XMonad.Layout.Named      (named)
import           XMonad.Layout.NoBorders  (noBorders)
import           XMonad.Layout.Spacing    (spacing)

import           Constants

myLayoutHook = avoidStruts (full ||| tiled ||| mtiled )
    where
         -- Fullscreen (default)
         full    = named "full" $ spacing tileSpacing $ noBorders Full
         -- Split vertically with phi as the ratio between the widths
         tiled   = named "tiled" $ spacing tileSpacing $ Tall 1 (5/100) (1/(toRational φ))
         -- Split horizonatlly in the same way
         mtiled  = named "mtiled" $ Mirror tiled


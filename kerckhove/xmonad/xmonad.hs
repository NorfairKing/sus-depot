{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.
-}


import           Prelude                  hiding (mod)
import           XMonad
import           XMonad.Config.Azerty
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Util.Scratchpad

import qualified XMonad.StackSet          as W


import           Actions
import           Constants
import           Keys
import           Layout
import           Workspaces


-- Color of focussed border
myFocusedBorderColor    ::  String
myFocusedBorderColor    =   colorMain

-- Color of inactive border
myNormalBorderColor     ::  String
myNormalBorderColor     =   colorSecondary




{- Xmonad Functionality -}


{- STARTUP -}
myStartupHook :: X ()
myStartupHook = do

    -- Make Java GUI's work
    setWMName "LG3D"

    spawn "redshift -l 50:0"

    -- Set the current workspace to the startup workspace
    windows $ W.greedyView startupWorkspace




{- MANAGE -}

-- Manage hook
myManageHook :: ManageHook
myManageHook = manageHook azertyConfig <+> composeAll myManagementHooks <+> manageDocks <+> manageScratchPad

{- Scratchpad -}
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.5     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%

myManagementHooks :: [ManageHook]
myManagementHooks =
                    [   className   =?  m           -->     move_to_mail    |   m   <-  mailClasses         ]   ++
                    [   className   =?  i           -->     move_to_web     |   i   <-  internet_classes    ]   ++
                    [
                    (title =? "Workflow")             -->     move_to_workflow
                    ]
                    where
                        move_to_mail        =   doF $ W.shift "w_m"
                        move_to_web         =   doF $ W.shift "w_i"
                        move_to_workflow    =   doF $ W.shift "w_w"


-- Stiching together all the settings
main :: IO()
main =
  xmonad $ withUrgencyHook NoUrgencyHook $ azertyConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , borderWidth = myBorderWidth
  , terminal = myTerminal
  , workspaces = myWorkspaces
  , modMask = mod
  , keys = myKeys
  , mouseBindings = myMouse
  , handleEventHook = fullscreenEventHook
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  }

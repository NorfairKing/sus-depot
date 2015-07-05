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
import qualified XMonad.StackSet          as W

import           Actions
import           Constants
import           Keys
import           Layout
import           Workspaces


{-
    I define where xmonad should send certain applications
-}
myManagementHooks :: [ManageHook]
myManagementHooks =
                    [   className   =?  m           -->     move_to_mail    |   m   <-  mailClasses         ]   ++
                    [   className   =?  i           -->     move_to_web     |   i   <-  internet_classes    ]   ++
                    [
                        (title =? "Workflow")       -->     move_to_workflow
                    ]
                    where
                        move_to_mail        =   doF $ W.shift mail_ws
                        move_to_web         =   doF $ W.shift web_ws
                        move_to_workflow    =   doF $ W.shift workflow_ws

-- | Startup
myStartupHook :: X ()
myStartupHook = do

    -- Make Java GUI's work
    setWMName "LG3D"

    spawn "redshift -l 50:0"

    -- Set the current workspace to the startup workspace
    windows $ W.greedyView startupWorkspace

myManageHook :: ManageHook
myManageHook = manageHook azertyConfig <+> composeAll myManagementHooks <+> manageDocks



-- Stiching together all the settings
main :: IO ()
main =
  xmonad $ withUrgencyHook NoUrgencyHook $ azertyConfig {
    focusedBorderColor = colorMain
  , normalBorderColor = colorSecondary
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

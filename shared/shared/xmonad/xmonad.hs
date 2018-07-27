{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.
-}

import           Prelude                  hiding (mod)
import           XMonad
import           XMonad.Config.Azerty
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import qualified XMonad.StackSet          as W
import           XMonad.Util.Run          (hPutStrLn, spawnPipe)

import           Actions
import           Constants
import           Keys
import           Layout
import           Workspaces

-- | Startup
myStartupHook :: X ()
myStartupHook = do

    -- Make Java GUI's work
    setWMName "LG3D"

    -- Set the current workspace to the startup workspace
    windows $ W.greedyView startupWorkspace

myManageHook :: ManageHook
myManageHook = manageHook azertyConfig <+> composeAll myManagementHooks <+> manageDocks

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



myTitleColor         =      colorMain           -- color of window title
myTitleLength        =      80                 -- truncate window title to this length
myCurrentWSColor     =      colorMain           -- color of active workspace
myVisibleWSColor     =      colorSecondary      -- color of inactive workspace
myUrgentWSColor      =      "#0000ff"           -- color of workspace with 'urgent' window
myCurrentWSLeft      =      "["                 -- wrap active workspace with these
myCurrentWSRight     =      "]"
myVisibleWSLeft      =      "("                 -- wrap inactive workspace with these
myVisibleWSRight     =      ")"
myUrgentWSLeft       =      "{"                 -- wrap urgent workspace with these
myUrgentWSRight      =      "}"



myLogHook xmproc = dynamicLogWithPP $ xmobarPP {
      ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
    , ppCurrent = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight
    , ppVisible = xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight
    , ppUrgent = xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight
  }




-- Stiching together all the settings
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  launch $ withUrgencyHook NoUrgencyHook $ azertyConfig {
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
  , logHook = myLogHook xmproc
  }

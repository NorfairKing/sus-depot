module Actions where

import           XMonad    (X (..), spawn)

import           Constants


-- Restart xmonad after recompiling it.
restart_xmonad :: X ()
restart_xmonad      = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

suspend :: X ()
suspend             = spawn "pm-suspend"

shutdown :: X ()
shutdown            = spawn "shutdown now"


term :: X ()
term                = spawn myTerminal

-- Workflow
workflow :: X ()
workflow            = spawn $ "emacsclient -c /home/syd/workflow/workflow.org"


-- Volume
mute, volumeUp, volumeDown :: X ()
mute                = spawn "amixer -q set Master 0%"
volumeDown          = spawn "amixer -q set Master 4%-"
volumeUp            = spawn "amixer -q set Master 4%+"


-- Brightness
lightDown, lightUp :: X ()
lightDown           = spawn "xbacklight -dec 10 -steps 1"
lightUp             = spawn "xbacklight -inc 10 -steps 1"


-- Screenshots
screenshot :: X ()
screenshot          = spawn "scrot"


module Actions
    (
    module Actions,
    module Internet
    )
    where

import           System.Exit     (exitSuccess)
import           XMonad          (X, io, spawn)
import           XMonad.Util.Run (unsafeSpawn)


import           Constants
import           Internet


-- Editors
editor              = "emacsclient -c"

-- Internet application
internet_classes   = ["Firefox","Google-chrome","Chromium"]

-- Mail application
mailClasses        = ["mutt"]
mail :: X ()
mail               = spawn "urxvt -e zsh -c \"mutt\""

-- Files application
files   :: X()
files              = spawn "nautilus --no-desktop"


-- Restart xmonad after recompiling it.
restart_xmonad :: X ()
restart_xmonad      = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

suspend :: X ()
suspend             = spawn "pm-suspend"

shutdown :: X ()
shutdown            = spawn "shutdown now"


logOut              :: X ()
logOut              = io exitSuccess


term :: String
term                = myTerminal

spawnTerminal       :: X ()
spawnTerminal       = spawn term


-- Workflow
workflow :: X ()
workflow            = unsafeSpawn $ "emacsclient -c $HOME/workflow/workflow.org"


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

-- Dmenu with custom settings
dmenu               :: X ()
dmenu               = spawn $ "dmenu_run -b -i -l 5 -nb '" ++ "#000000" ++ "' -nf '" ++ colorSecondary ++ "' -sb '" ++ "#000000" ++ "' -sf '" ++ colorMain ++ "'"


-- to define placeholders
nothing             :: X ()
nothing             = return ()



module Actions
    (
    module Actions,
    module Internet
    )
    where

import           System.Exit                    (exitSuccess)
import           XMonad                         (ChangeLayout (..),
                                                 IncMasterN (..), Resize (..),
                                                 X, io, kill, sendMessage,
                                                 spawn, windows, withFocused)
import qualified XMonad.StackSet                as W

import           XMonad.Actions.PhysicalScreens
import           XMonad.Prompt                  (XPConfig (..), def,
                                                 defaultXPConfig)
import           XMonad.Prompt.Input
import           XMonad.Util.Run                (unsafeSpawn)

import           Constants
import           Internet
import           Solarized


-- Editors
editor              = "emacsclient -c"

-- Internet application
internet_classes    = ["Firefox","Google-chrome","Chromium"]

-- Mail application
mailClasses         = ["mutt"]

mail :: X ()
mail                = spawn "urxvt -e zsh -c \"mutt\""

-- Files application
files :: X ()
files               = spawn "nautilus --no-desktop"

suspend :: X ()
suspend = spawn "pm-suspend"

shutdown :: X ()
shutdown = spawn "shutdown now"


logOut :: X ()
logOut = io exitSuccess


term :: String
term = myTerminal

spawnTerminal :: X ()
spawnTerminal = spawn term


-- Workflow
org :: FilePath -> X ()
org fp = unsafeSpawn $ "emacsclient -c " ++ fp

orgInbox :: X ()
orgInbox = org "$ORG_INBOX"

orgWork :: X ()
orgWork = org "$ORG_WORK"

orgPersonal :: X ()
orgPersonal = org "$ORG_PERSONAL"

orgProject :: X ()
orgProject = org "$ORG_PROJECT"

orgWeeklyReview :: X ()
orgWeeklyReview = org "$ORG_WEEKLY_REVIEW"

orgSomedayMaybe :: X ()
orgSomedayMaybe = org "$ORG_SOMEDAY_MAYBE"

orgBlog :: X ()
orgBlog = org "$ORG_BLOG"




-- Volume
mute, volumeUp, volumeDown :: X ()
mute       = spawn "amixer -q set PCM 0%"
volumeDown = spawn "amixer -q set PCM 4%-"
volumeUp   = spawn "amixer -q set PCM 4%+"


-- Brightness
lightDown, lightUp :: X ()
lightDown = spawn "xbacklight -dec 10 -steps 1"
lightUp   = spawn "xbacklight -inc 10 -steps 1"


-- Screenshots
screenshot :: X ()
screenshot = spawn "scrot"

-- Dmenu with custom settings
dmenu :: X ()
dmenu = spawn $ "dmenu_run -b -i -l 5 -nb '" ++ "#000000" ++ "' -nf '" ++ colorSecondary ++ "' -sb '" ++ "#000000" ++ "' -sf '" ++ colorMain ++ "'"


-- to define placeholders
nothing :: X ()
nothing = return ()


-- Select the next layout.
nextLayout :: X ()
nextLayout = sendMessage NextLayout

-- Select the next window.
nextWindow :: X ()
nextWindow = windows W.focusDown

-- Select the previous window.
previousWindow :: X ()
previousWindow = windows W.focusUp

-- Close the selected window
closeWindow :: X ()
closeWindow = kill

-- Shrink the master window.
shrinkWindow :: X ()
shrinkWindow = sendMessage Shrink

-- Expand the master window.
expandWindow :: X ()
expandWindow = sendMessage Expand

focusWindowUp :: X ()
focusWindowUp = windows W.focusUp

swapWindowUp :: X ()
swapWindowUp = windows W.swapUp

focusNextScreen :: X ()
focusNextScreen = onNextNeighbour W.view

focusPrevScreen :: X ()
focusPrevScreen = onPrevNeighbour W.view

shiftNextScreen :: X ()
shiftNextScreen = onNextNeighbour W.shift

shiftPrevScreen :: X ()
shiftPrevScreen = onPrevNeighbour W.shift

-- Select the previous window.
focusWindowDown :: X ()
focusWindowDown = windows W.focusDown

-- Swap the selected window with the previous window.
swapWindowDown :: X ()
swapWindowDown = windows W.swapDown

-- Select the master window
focusMaster :: X ()
focusMaster = windows W.focusMaster

-- Swap the slected window with the master window
swapMaster :: X ()
swapMaster = windows W.swapMaster

-- Push selected window back into tiling
tileAgain :: X ()
tileAgain = withFocused $ windows . W.sink

-- Increment the number of windows in the master area.
moreWindows :: X ()
moreWindows = sendMessage (IncMasterN 1)

-- Decrement the number of windows in the master area.
lessWindows :: X ()
lessWindows = sendMessage (IncMasterN (-1))


myXPConfig :: XPConfig
myXPConfig = def {
                font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
              , height      = 22
              , bgColor     = solarizedBase03
              , fgColor     = solarizedBase1
              , bgHLight    = solarizedYellow
              , fgHLight    = solarizedBase02
              , borderColor = solarizedYellow
            }

inPrompt :: X ()
inPrompt = inputPrompt myXPConfig "in" ?+ (\s -> unsafeSpawn $ "inray add " ++ s)

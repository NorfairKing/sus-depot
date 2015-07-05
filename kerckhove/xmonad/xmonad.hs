{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.
-}

import           System.Exit

import           XMonad
import           XMonad.Actions.Plane
import           XMonad.Config.Azerty
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad

import qualified Data.Map                 as M
import qualified XMonad.StackSet          as W

import           XMonad.Prompt
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.Shell

import qualified XMonad.Actions.Search    as S
import qualified XMonad.Actions.Submap    as SM
import qualified XMonad.Prompt            as P

import           Actions
import           Constants

import           Layout
import           Workspaces

{- APPEARANCE -}

{- Colors -}

-- Color of focussed border
myFocusedBorderColor    ::  String
myFocusedBorderColor    =   colorMain

-- Color of inactive border
myNormalBorderColor     ::  String
myNormalBorderColor     =   colorSecondary



{- Mod Key -}

myModMask               ::  KeyMask
myModMask               =   mod4Mask

-- Mail application


myXPConfig          :: XPConfig
myXPConfig          = defaultXPConfig {   font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
                                        , height=22}

scratchPad          :: X ()
scratchPad          = scratchpadSpawnActionTerminal myTerminal



{- Search -}

-- Perform search on prompted input.
promptedSearch      :: X ()
promptedSearch      = SM.submap (searchEngineMap $ S.promptSearch P.defaultXPConfig)

-- Perform search on selected text.
selectedSearch      :: X ()
selectedSearch      = SM.submap (searchEngineMap S.selectSearch)

-- Keys to perform searches
searchEngineMap     :: (S.SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
searchEngineMap method = M.fromList
       [
         ((0, xK_b), method $ S.intelligent S.google)
       , ((0, xK_g), method S.google)
       , ((0, xK_h), method S.hoogle)
       , ((0, xK_i), method S.images)
       , ((0, xK_m), method S.maps)
       , ((0, xK_w), method S.wikipedia)
       , ((0, xK_y), method S.youtube)
       ]



{- Keyboard Layout -}

-- Change keyboard layout.
changeKeyboardLayout :: X ()
changeKeyboardLayout = SM.submap keyboardMap

-- Keys to change keyboard layout
keyboardMap :: M.Map (KeyMask, KeySym) (X ())
keyboardMap = M.fromList
    [
          ((0, xK_F1), spawn "setxkbmap us")
        , ((0, xK_F2), spawn "setxkbmap be")
        , ((0, xK_F3), spawn "setxkbmap dvorak")
        , ((0, xK_F4), spawn "setxkbmap us -variant dvp")
        , ((0, xK_F5), spawn "xmodmap ~/.keyboards/dvorak.kinesis")
        , ((0, xK_F12), spawn "xset r rate 250 30")
    ]



{- Xmonad Functionality -}

-- Select the next layout.
nextLayout      :: X ()
nextLayout      = sendMessage NextLayout

-- Select the next window.
nextWindow      :: X()
nextWindow      = windows W.focusDown

-- Select the previous window.
previousWindow  :: X ()
previousWindow  = windows W.focusUp

-- Close the selected window
closeWindow     :: X ()
closeWindow     = kill

-- Shrink the master window.
shrinkWindow    :: X ()
shrinkWindow    = sendMessage Shrink

expandWindow    :: X ()
expandWindow    = sendMessage Expand

focusWindowUp   :: X ()
focusWindowUp   = windows W.focusUp

swapWindowUp    :: X ()
swapWindowUp    = windows W.swapUp

focusWindowDown :: X ()
focusWindowDown = windows W.focusDown

swapWindowDown  :: X ()
swapWindowDown  = windows W.swapDown

-- Push selected window back into tiling
tileAgain       :: X ()
tileAgain       = withFocused $ windows . W.sink

-- Increment the number of windows in the master area.
moreWindows     :: X ()
moreWindows     = sendMessage (IncMasterN 1)

-- Decrement the number of windows in the master area.
lessWindows     :: X ()
lessWindows     = sendMessage (IncMasterN (-1))

{- KEYBINDINGS -}
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [
        ((myModMask                                 , xK_f      ),  spawnTerminal                   ),
        ((myModMask                                 , xK_y      ),  closeWindow                     ),

        -- [{+(= *)!}]
        ((myModMask                                 , xK_bracketleft    ),  nothing                 ),
        ((myModMask                                 , xK_braceleft      ),  focusWindowDown         ),
        ((myModMask                                 , xK_plus           ),  lessWindows             ),
        ((myModMask                                 , xK_parenleft      ),  focusWindowDown         ),
        ((myModMask .|. shiftMask                   , xK_parenleft      ),  swapWindowDown          ),
        ((myModMask                                 , xK_equal          ),  shrinkWindow            ),
        ((myModMask                                 , xK_asterisk       ),  expandWindow            ),
        ((myModMask                                 , xK_parenright     ),  focusWindowUp           ),
        ((myModMask .|. shiftMask                   , xK_parenright     ),  swapWindowUp            ),
        ((myModMask                                 , xK_exclam         ),  moreWindows             ),
        ((myModMask                                 , xK_braceright     ),  focusWindowUp           ),
        ((myModMask                                 , xK_bracketright   ),  nothing                 ),

        ((myModMask                                 , xK_Return ),  promptedSearch                  ),
        ((myModMask .|. shiftMask                   , xK_Return ),  changeKeyboardLayout            ),
        ((myModMask .|. controlMask                 , xK_Return ),  logOut                          ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Return ),  nothing                         ),
        ((myModMask                                 , xK_space  ),  nextLayout                      ),
        ((myModMask                                 , xK_Tab    ),  nextWindow                      ),
        ((myModMask .|. shiftMask                   , xK_Tab    ),  previousWindow                  ),
        ((myModMask .|. controlMask                 , xK_q      ),  suspend                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_q      ),  shutdown                        ),
        ((myModMask .|. controlMask                 , xK_x      ),  shellPrompt myXPConfig          ),
        ((myModMask                                 , xK_comma  ),  workflow                        ),
        ((myModMask                                 , xK_period ),  internet                        ),
        ((myModMask                                 , xK_F4     ),  spawn mail                      ),
        ((myModMask                                 , xK_F5     ),  lightDown                       ),
        ((myModMask                                 , xK_F6     ),  lightUp                         ),
        ((myModMask                                 , xK_F10    ),  mute                            ),
        ((myModMask                                 , xK_F11    ),  volumeDown                      ),
        ((myModMask .|. shiftMask                   , xK_F11    ),  lightDown                       ),
        ((myModMask                                 , xK_F12    ),  volumeUp                        ),
        ((myModMask .|. shiftMask                   , xK_F12    ),  lightUp                         ),
        ((myModMask                                 , xK_BackSpace  ),  tileAgain                   )
    ]
    ++
    navigationKeys

-- Switch to the letter of a workspace by pressing mod + letter
navigationKeys :: [((KeyMask, KeySym), X ())]
navigationKeys =
    [
        (
            (mask .|. myModMask, key)
            ,
            windows $ switch workspace
        )
        |
        (workspace, key) <- zip myWorkspaces workspaceKeys,
        (switch, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]




-- | Mouse bindings: default actions bound to mouse events
myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse (XConfig {XMonad.modMask = myModMask}) = M.fromList
    [
        -- Left_mouse_button    Set the window to floating mode and mov by dragging
        ((myModMask                                 , button1   ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster      ),
        -- Middle_mouse_button  Do a fancy search
        ((myModMask                                 , button2   ), \_ -> selectedSearch                                             ),
        -- Right_mouse_button   Set the window to floating mode and resize by dragging
        ((myModMask                                 , button3   ), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)  )
    ]




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
  , modMask = myModMask
  , keys = myKeys
  , mouseBindings = myMouse
  , handleEventHook = fullscreenEventHook
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  }

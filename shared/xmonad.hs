{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.
-}

import qualified Data.Map                 as M
import           Prelude                  hiding (mod)
import           System.Exit
import           XMonad
import           XMonad.Actions.Plane
import qualified XMonad.Actions.Search    as S
import qualified XMonad.Actions.Submap    as SM
import           XMonad.Config.Azerty
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import qualified XMonad.Prompt            as P
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet          as W


-- | Colors
colorMain, colorSecondary   ::  String
colorMain                   =   "#268BD2"
colorSecondary              =   "#657b83"


-- | Mod mask
mod                         ::  KeyMask
mod                         =   mod4Mask

-- | Color of focussed border
myFocusedBorderColor        ::  String
myFocusedBorderColor        =   colorMain

-- | Color of inactive border
myNormalBorderColor         ::  String
myNormalBorderColor         =   colorSecondary

-- | Terminal emulator
myTerminal                  ::  String
myTerminal                  =   "urxvt"

-- | The with of the borders between windows
myBorderWidth               ::  Dimension
myBorderWidth               =   1

-- | Space between tiles
tileSpacing                 :: Int
tileSpacing                 = 3


-- | Workspaces, a 3x3 grid of workspaces
bl  =   "1: Workflow"       -- Bottom   Left
bm  =   "2: Etc"            -- Bottom   Middle
br  =   "3: Mail"           -- Bottom   Right
ml  =   "4: Terminal"       -- Middle   Left
mm  =   "5: Development"    -- Middle   Middle
mr  =   "6: Internet"       -- Middle   Right
tl  =   "7: Chat"           -- Top      Left
tm  =   "8: Etc"            -- Top      Middle
tr  =   "9: Clip"           -- Top      Right

-- | Where to send applications
workflow_ws = bl
mail_ws     = br
web_ws      = mr

-- | The workspaces list
myWorkspaces :: [WorkspaceId]
myWorkspaces =
  [
    tl, tm, tr,
    ml, mm, mr,
    bl, bm, br
  ]

-- | The workspace that will be on screen after launch
startupWorkspace = mr

-- | Layouts
-- You really don't want to see the type of this function!
myLayoutHook = avoidStruts (full ||| tiled ||| mtiled )
    where
        -- Fullscreen (default)
        full    = named "full" $ spacing tileSpacing $ noBorders Full
        -- Split vertically with phi as the ratio between the widths
        tiled   = named "tiled" $ spacing tileSpacing $ Tall 1 (5/100) (1/(toRational phi))
        -- Split horizonatlly in the same way
        mtiled  = named "mtiled" $ Mirror tiled
        -- The golden ratio
        phi :: Double
        phi = (1 + sqrt 5) / 2.0


-- Restart xmonad after recompiling it.
restart_xmonad      = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- States
suspend             = spawn "pm-suspend"
shutdown            = spawn "shutdown now"

-- Spawn a terminal
term :: X ()
term                = spawn myTerminal

-- Editors
editor              = "emacsclient -c"

-- Workflow
workflow            = spawn $ editor ++ " /home/syd/workflow/workflow.org"

-- Dmenu with custom settings
dmenu               = spawn $ "dmenu_run -b -i -l 5 -nb '" ++ "#000000" ++ "' -nf '" ++ colorSecondary ++ "' -sb '" ++ "#000000" ++ "' -sf '" ++ colorMain ++ "'"

-- Internet application
internet_classes   = ["Firefox"]
internet           = spawn "firefox"

-- Mail application
mailClasses        = ["mutt"]
mail               = spawn "urxvt -e zsh -c \"mutt\""

-- Files application
files              = spawn "nautilus --no-desktop"

-- Brightness
lightDown          = spawn "xbacklight -dec 10 -steps 1"
lightUp            = spawn "xbacklight -inc 10 -steps 1"

-- Volume
mute               = spawn "amixer -q set Master 0%"
volumeDown         = spawn "amixer -q set Master 4%-"
volumeUp           = spawn "amixer -q set Master 4%+"


-- Take a screenshot
screenshotEntire   = spawn "scrot"


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*", height=22}

searchEntered :: X ()
searchEntered = SM.submap (searchEngineMap $ S.promptSearch P.defaultXPConfig)

searchSelected :: X ()
searchSelected = SM.submap (searchEngineMap S.selectSearch)

searchEngineMap :: (S.SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
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

keyboardMap :: M.Map (KeyMask, KeySym) (X ())
keyboardMap = M.fromList
    [
          ((0, xK_F1), spawn "setxkbmap us")
        , ((0, xK_F2), spawn "setxkbmap be")
        , ((0, xK_F3), spawn "setxkbmap dvorak")
        , ((0, xK_F4), spawn "setxkbmap us -variant dvp")
        , ((0, xK_F4), spawn "xmodmap ~/.keyboards/dvorak.primus")
    ]


-- custom keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [
        ((mod .|. shiftMask                   , xK_Return ),  term                                ) -- Spawn my terminal.
    ,   ((mod .|. controlMask .|. shiftMask   , xK_Return ),  SM.submap keyboardMap               )
    ,   ((mod                                 , xK_space  ),  sendMessage NextLayout              ) -- Select the next layout.
    ,   ((mod .|. shiftMask                   , xK_space  ),  setLayout $ XMonad.layoutHook conf  ) -- Select the first layout.
    ,   ((mod                                 , xK_Tab    ),  windows W.focusDown                 ) -- Select the next window.
    ,   ((mod .|. shiftMask                   , xK_Tab    ),  windows W.focusUp                   ) -- Select the previous window.
    ,   ((mod .|. shiftMask                   , xK_c      ),  kill                                ) -- Close the selected window.
    ,   ((mod                                 , xK_d      ),  dmenu                               ) -- Show my dmenu.
    ,   ((mod                                 , xK_f      ),  files                               ) -- Open my file explorer.
    ,   ((mod                                 , xK_h      ),  sendMessage Shrink                  ) -- Shrink the master window.
    ,   ((mod                                 , xK_i      ),  internet                            ) -- Open my internet browser.
    ,   ((mod                                 , xK_j      ),  windows W.focusDown                 ) -- Select the previous window.
    ,   ((mod .|. shiftMask                   , xK_j      ),  windows W.swapDown                  ) -- Swap the selected window with the previous window.
    ,   ((mod                                 , xK_k      ),  windows W.focusUp                   ) -- Select the next window.
    ,   ((mod .|. shiftMask                   , xK_k      ),  windows W.swapUp                    ) -- Swap the selected window with the next window.
    ,   ((mod                                 , xK_l      ),  sendMessage Expand                  ) -- Expand the master window.
    ,   ((mod                                 , xK_m      ),  windows W.focusMaster               ) -- Select the master window.
    ,   ((mod .|. shiftMask                   , xK_m      ),  windows W.swapMaster                ) -- Swap the selected window with the master window.
    ,   ((mod                                 , xK_n      ),  refresh                             ) -- Resize viewed windows to the correct size
    ,   ((mod                                 , xK_q      ),  restart_xmonad                      ) -- Recompile and restart Xmonad.
    ,   ((mod .|. shiftMask                   , xK_q      ),  io exitSuccess                      ) -- Log out.
    ,   ((mod .|. controlMask                 , xK_q      ),  suspend                             ) -- Suspend.
    ,   ((mod .|. controlMask .|. shiftMask   , xK_q      ),  shutdown                            ) -- Shut down
    ,   ((mod                                 , xK_s      ),  searchEntered                       )
    ,   ((mod .|. shiftMask                   , xK_s      ),  searchSelected                      )
    ,   ((mod                                 , xK_t      ),  withFocused $ windows . W.sink      ) -- Push selected window back into tiling
    ,   ((mod                                 , xK_u      ),  focusUrgent                         ) -- Select the most recently urgent window
    ,   ((mod                                 , xK_x      ),  screenshotEntire                    ) -- Take a screenshot
    ,   ((mod .|. controlMask                 , xK_x      ),  shellPrompt myXPConfig              )
    ,   ((mod                                 , xK_comma  ),  sendMessage (IncMasterN 1)          ) -- Increment the number of windows in the master area.
    ,   ((mod .|. shiftMask                   , xK_comma  ),  sendMessage (IncMasterN (-1))       ) -- Decrement the number of windows in the master area.
    ,   ((mod                                 , xK_F3     ),  workflow                            ) -- Open my workflow with my editor.
    ,   ((mod                                 , xK_F4     ),  mail                                ) -- Open my mail client.
    ,   ((mod                                 , xK_F5     ),  lightDown                           ) -- Decrease the brightness of the screen.
    ,   ((mod                                 , xK_F6     ),  lightUp                             ) -- Increase the brightness of the screen.
    ,   ((mod                                 , xK_F10    ),  mute                                ) -- Mute the volume.
    ,   ((mod                                 , xK_F11    ),  volumeDown                          ) -- Decrease the volume.
    ,   ((mod                                 , xK_F12    ),  volumeUp                            ) -- Increase the volume.
    ]
    ++ workspaceNavigation

-- | Shortcuts for navigating workspaces
workspaceNavigation :: [((KeyMask, KeySym), X ())]
workspaceNavigation =
    -- Navigate directly
    [
        (
            (m .|. mod, k)
            ,
            windows $ f i
        )
        |
        (i, k) <- zip myWorkspaces numPadKeys ++ zip myWorkspaces numKeys,
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
        ++
    -- Navigate with arrow keys
    M.toList (planeKeys mod (Lines 4) Finite)


-- | Mouse bindings
myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse (XConfig {XMonad.modMask = m}) = M.fromList
    [
        -- Left_mouse_button    Set the window to floating mode and move by dragging
        ((m , button1   ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster      )
        -- Right_mouse_button   Raise the window to the top of the stack
    ,   ((m , button2   ), windows . (W.shiftMaster .) . W.focusWindow                      )
        -- Middle_mouse_button  Set the window to floating mode and resize by dragging
    ,   ((m , button3   ), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster    )
    ]

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


{-
    I define the shortcuts to navigate my workspaces, such that they work with or without numlock on.
-}

numPadKeys :: [KeySym]
numPadKeys =
  [
    xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up,
    xK_KP_Left, xK_KP_Begin, xK_KP_Right,
    xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down
  ]

numKeys :: [KeySym]
numKeys =
  [
    xK_7, xK_8, xK_9,
    xK_4, xK_5, xK_6,
    xK_1, xK_2, xK_3
  ]

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

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
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
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


-- Colors
colorMain, colorSecondary   ::  String
colorMain                   =   "#268BD2"
colorSecondary              =   "#657b83"


-- Mod mask
myModMask               ::  KeyMask
myModMask               =   mod4Mask

-- Color of focussed border
myFocusedBorderColor    ::  String
myFocusedBorderColor    =   colorMain

-- Color of inactive border
myNormalBorderColor     ::  String
myNormalBorderColor     =   colorSecondary

-- Terminal emulator
myTerminal              ::  String
myTerminal              =   "urxvt"

-- The with of the borders between windows
myBorderWidth           ::  Dimension
myBorderWidth           =   1




{-
    I define my workspaces,
    They are a 3x3 grid of workspaces
-}

bl  =   "1: Workflow"       -- Bottom   Left
bm  =   "2: Chat"           -- Bottom   Middle
br  =   "3: Mail"           -- Bottom   Right
ml  =   "4: Terminal"       -- Middle   Left
mm  =   "5: Development"    -- Middle   Middle
mr  =   "6: Internet"       -- Middle   Right
tl  =   "7: Skype"          -- Top      Left
tm  =   "8: Etc"            -- Top      Middle
tr  =   "9: Clip"           -- Top      Right

-- define where to send applications
workflow_ws = bl
paula_ws    = bm
mail_ws     = br
web_ws      = mr
todo_ws     = tm
journal_ws  = tl
chat_ws     = tl

-- The workspaces list
myWorkspaces :: [WorkspaceId]
myWorkspaces =
  [
    tl, tm, tr,
    ml, mm, mr,
    bl, bm, br
  ]

-- | The workspace that will bee on screen after launch
startupWorkspace = bm




-- | Layouts
myLayoutHook = avoidStruts (full ||| tiled ||| mtiled )
    where
        -- Fullscreen (default)
        full    = named "full" $ spacing 3 $ noBorders Full
        -- Split vertically with phi as the ratio between the widths
        tiled   = named "tiled" $ spacing 3 $ Tall 1 (5/100) (2/(1+(toRational(sqrt 5 ::Double))))
        -- Split horizonatlly in the same way
        mtiled  = named "mtiled" $ Mirror tiled



{-
    I define my own applications
-}

restart_xmonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- States
suspend             = "pm-suspend"
shutdown            = "shutdown now"

-- Terminals
term                :: String
term                = "urxvt"

terminalWTitle          :: String -> String
terminalWTitle title    = term ++ "-title " ++ title ++ " "

-- Editors
editor              = "emacsclient -c"
editor2             = "vim"

-- Workflow
workflow            = editor ++ " /home/syd/workflow/workflow.txt"

-- Dmenu with custom settings
dmenu               = "dmenu_run -b -i -l 5 -nb '" ++ "#000000" ++ "' -nf '" ++ colorSecondary ++ "' -sb '" ++ "#000000" ++ "' -sf '" ++ colorMain ++ "'"

-- Internet application
internet_classes    = ["Firefox"]
internet            = "firefox"

-- Mail application
mailClasses        = ["mutt"]
mail                = "urxvt -e zsh -c \"mutt\""

-- Files application
files               = "nautilus --no-desktop"

-- Scanner
scanner             = "scangearmp"

-- Brightness
lightDown          = "xbacklight -dec 10 -steps 1"
lightUp            = "xbacklight -inc 10 -steps 1"

-- Volume
mute                = "amixer -q set Master 0%"
volumeDown         = "amixer -q set Master 4%-"
volumeUp           = "amixer -q set Master 4%+"

-- Latex
texmaker            = "texmaker"

-- to define placeholders
nothing             = "echo placeholder"


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
                             ,height=22}

scratchPad :: X ()
scratchPad = scratchpadSpawnActionTerminal myTerminal

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
        ((myModMask                                 , xK_Return ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_Return ),  spawn myTerminal                    ), -- Spawn my terminal.
        ((myModMask .|. controlMask                 , xK_Return ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Return ),  SM.submap keyboardMap               ),

        ((myModMask                                 , xK_space  ),  sendMessage NextLayout              ), -- Select the next layout.
        ((myModMask .|. shiftMask                   , xK_space  ),  setLayout $ XMonad.layoutHook conf  ), -- Select the first layout.
        ((myModMask .|. controlMask                 , xK_space  ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_space  ),  spawn nothing                       ),

        ((myModMask                                 , xK_Tab    ),  windows W.focusDown                 ), -- Select the next window.
        ((myModMask .|. shiftMask                   , xK_Tab    ),  windows W.focusUp                   ), -- Select the previous window.
        ((myModMask .|. controlMask                 , xK_Tab    ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Tab    ),  spawn nothing                       ),

        ((myModMask                                 , xK_a      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_a      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_a      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_a      ),  spawn nothing                       ),

        ((myModMask                                 , xK_b      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_b      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_b      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_b      ),  spawn nothing                       ),

        ((myModMask                                 , xK_c      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_c      ),  kill                                ), -- Close the selected window.
        ((myModMask .|. controlMask                 , xK_c      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_c      ),  spawn nothing                       ),

        ((myModMask                                 , xK_d      ),  spawn dmenu                         ), -- Show my dmenu.
        ((myModMask .|. shiftMask                   , xK_d      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_d      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_d      ),  spawn nothing                       ),

        ((myModMask                                 , xK_e      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_e      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_e      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_e      ),  spawn nothing                       ),

        ((myModMask                                 , xK_f      ),  spawn files                         ), -- Open my file explorer.
        ((myModMask .|. shiftMask                   , xK_f      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_f      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_f      ),  spawn nothing                       ),

        ((myModMask                                 , xK_g      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_g      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_g      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_g      ),  spawn nothing                       ),

        ((myModMask                                 , xK_h      ),  sendMessage Shrink                  ), -- Shrink the master window.
        ((myModMask .|. shiftMask                   , xK_h      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_h      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_h      ),  spawn nothing                       ),

        ((myModMask                                 , xK_i      ),  spawn internet                      ), -- Open my internet browser.
        ((myModMask .|. shiftMask                   , xK_i      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_i      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_i      ),  spawn nothing                       ),

        ((myModMask                                 , xK_j      ),  windows W.focusDown                 ), -- Select the previous window.
        ((myModMask .|. shiftMask                   , xK_j      ),  windows W.swapDown                  ), -- Swap the selected window with the previous window.
        ((myModMask .|. controlMask                 , xK_j      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_j      ),  spawn nothing                       ),

        ((myModMask                                 , xK_k      ),  windows W.focusUp                   ), -- Select the next window.
        ((myModMask .|. shiftMask                   , xK_k      ),  windows W.swapUp                    ), -- Swap the selected window with the next window.
        ((myModMask .|. controlMask                 , xK_k      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_k      ),  spawn nothing                       ),

        ((myModMask                                 , xK_l      ),  sendMessage Expand                  ), -- Expand the master window.
        ((myModMask .|. shiftMask                   , xK_l      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_l      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_l      ),  spawn nothing                       ),

        ((myModMask                                 , xK_m      ),  windows W.focusMaster               ), -- Select the master window.
        ((myModMask .|. shiftMask                   , xK_m      ),  windows W.swapMaster                ), -- Swap the selected window with the master window.
        ((myModMask .|. controlMask                 , xK_m      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_m      ),  spawn nothing                       ),

        ((myModMask                                 , xK_n      ),  refresh                             ), -- Resize viewed windows to the correct size
        ((myModMask .|. shiftMask                   , xK_n      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_n      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_n      ),  spawn nothing                       ),

        ((myModMask                                 , xK_o      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_o      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_o      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_o      ),  spawn nothing                       ),

        ((myModMask                                 , xK_p      ),  scratchPad                       ),
        ((myModMask .|. shiftMask                   , xK_p      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_p      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_p      ),  spawn nothing                       ),

        ((myModMask                                 , xK_q      ),  restart_xmonad                      ), -- Recompile and restart Xmonad.
        ((myModMask .|. shiftMask                   , xK_q      ),  io exitSuccess           ), -- Log out.
        ((myModMask .|. controlMask                 , xK_q      ),  spawn suspend                       ), -- Suspend.
        ((myModMask .|. controlMask .|. shiftMask   , xK_q      ),  spawn shutdown                      ), -- Shut down

        ((myModMask                                 , xK_r      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_r      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_r      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_r      ),  spawn nothing                       ),

        ((myModMask                                 , xK_s      ),  SM.submap (searchEngineMap $ S.promptSearch P.defaultXPConfig)),
        ((myModMask .|. shiftMask                   , xK_s      ),  SM.submap (searchEngineMap S.selectSearch)),
        ((myModMask .|. controlMask                 , xK_s      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_s      ),  spawn nothing                       ),

        ((myModMask                                 , xK_t      ),  withFocused $ windows . W.sink      ), -- Push selected window back into tiling
        ((myModMask .|. shiftMask                   , xK_t      ),  spawn texmaker                      ), -- Open texmaker
        ((myModMask .|. controlMask                 , xK_t      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_t      ),  spawn nothing                       ),

        ((myModMask                                 , xK_u      ),  focusUrgent                         ), -- Select the most recently urgent window
        ((myModMask .|. shiftMask                   , xK_u      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_u      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_u      ),  spawn nothing                       ),

        ((myModMask                                 , xK_v      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_v      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_v      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_v      ),  spawn nothing                       ),

        ((myModMask                                 , xK_w      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_w      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_w      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_w      ),  spawn nothing                       ),

        ((myModMask                                 , xK_x      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_x      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_x      ),  shellPrompt myXPConfig              ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_x      ),  spawn nothing                       ),

        ((myModMask                                 , xK_y      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_y      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_y      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_y      ),  spawn nothing                       ),

        ((myModMask                                 , xK_z      ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_z      ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_z      ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_z      ),  spawn nothing                       ),

        ((myModMask                                 , xK_comma  ),  sendMessage (IncMasterN 1)          ), -- Increment the number of windows in the master area.
        ((myModMask .|. shiftMask                   , xK_comma  ),  sendMessage (IncMasterN (-1))       ), -- Decrement the number of windows in the master area.
        ((myModMask .|. controlMask                 , xK_comma  ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_comma  ),  spawn nothing                       ),

        ((myModMask                                 , xK_period ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_period ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_period ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_period ),  spawn nothing                       ),

        ((myModMask                                 , xK_F1     ),  spawn nothing                       ), -- Open my journal with my editor.
        ((myModMask .|. shiftMask                   , xK_F1     ),  spawn nothing                       ), -- Open my journal with my other editor.
        ((myModMask .|. controlMask                 , xK_F1     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F1     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F2     ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F2     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F2     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F2     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F3     ),  spawn workflow                      ), -- Open my workflow with my editor.
        ((myModMask .|. shiftMask                   , xK_F3     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F3     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F3     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F4     ),  spawn mail                          ), -- Open my mail client.
        ((myModMask .|. shiftMask                   , xK_F4     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F4     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F4     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F5     ),  spawn lightDown                     ), -- Decrease the brightness of the screen.
        ((myModMask .|. shiftMask                   , xK_F5     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F5     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F5     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F6     ),  spawn lightUp                       ), -- Increase the brightness of the screen.
        ((myModMask .|. shiftMask                   , xK_F6     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F6     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F6     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F7     ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F7     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F7     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F7     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F8     ),  spawn nothing                       ), -- Open my chat client.
        ((myModMask .|. shiftMask                   , xK_F8     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F8     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F8     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F9     ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F9     ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F9     ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F9     ),  spawn nothing                       ),

        ((myModMask                                 , xK_F10    ),  spawn mute                          ), -- Mute the volume.
        ((myModMask .|. shiftMask                   , xK_F10    ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F10    ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F10    ),  spawn nothing                       ),

        ((myModMask                                 , xK_F11    ),  spawn volumeDown                    ), -- Decrease the volume.
        ((myModMask .|. shiftMask                   , xK_F11    ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F11    ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F11    ),  spawn nothing                       ),

        ((myModMask                                 , xK_F12    ),  spawn volumeUp                      ), -- Increase the volume.
        ((myModMask .|. shiftMask                   , xK_F12    ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_F12    ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F12    ),  spawn nothing                       ),

        ((myModMask                                 , xK_plus   ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_plus   ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_plus   ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_plus   ),  spawn nothing                       ),

        ((myModMask                                 , xK_minus  ),  spawn nothing                       ),
        ((myModMask .|. shiftMask                   , xK_minus  ),  spawn nothing                       ),
        ((myModMask .|. controlMask                 , xK_minus  ),  spawn nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_minus  ),  spawn nothing                       )
    ]
        ++ -- Shortcuts for navidating workspaces
    [
        ((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
        ++
    [
        ((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
        ++
    M.toList (planeKeys myModMask (Lines 4) Finite)
        ++
    [
        ((m .|. myModMask, key), screenWorkspace sc
        >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]


-- | Mouse bindings: default actions bound to mouse events
myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse (XConfig {XMonad.modMask = myModMask}) = M.fromList
    [
        -- Left_mouse_button    Set the window to floating mode and move by dragging
        ((myModMask                                 , button1   ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster      ),
        -- Right_mouse_button   Raise the window to the top of the stack
        ((myModMask                                 , button2   ), windows . (W.shiftMaster .) . W.focusWindow                      ),
        -- Middle_mouse_button  Set the window to floating mode and resize by dragging
        ((myModMask                                 , button3   ), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster    )--,
        ---- Scroll_down          Nothing
        --((myModMask                                 , button4   ), spawn nothing                                                    ),
        ---- Scroll_up            Nothing
        --((myModMask                                 , button5   ), spawn nothing                                                    )
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
myManageHook = manageHook azertyConfig <+> composeAll myManagementHooks <+> manageDocks <+> manageScratchPad

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.2     -- terminal height, 20%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 80%
    l = 1 - w   -- distance from left edge, 0%



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

{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.
-}

import System.Exit

import XMonad
import XMonad.Actions.Plane
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile

import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

{- APPEARANCE -}

{- Colors -}

-- Dynamic color definitions
colorMain, colorSecondary   ::  String
colorMain                   =   "#268BD2"
colorSecondary              =   "#657b83"

-- Color of focussed border
myFocusedBorderColor    ::  String
myFocusedBorderColor    =   colorMain

-- Color of inactive border
myNormalBorderColor     ::  String
myNormalBorderColor     =   colorSecondary



{- Borders -}

-- The with of the borders between windows
myBorderWidth           ::  Dimension
myBorderWidth           =   1



{- Mod Key -}

myModMask               ::  KeyMask
myModMask               =   mod4Mask



{- Terminal -}

myTerminal              ::  String
myTerminal              =   "urxvt"




{- LAYOUT -}

myLayoutHook = avoidStruts (full ||| tiled ||| mtiled )
    where
        -- Fullscreen (default)
        full    = named "full" $ spacing 3 $ noBorders Full
        -- Split vertically with phi as the ratio between the widths
        tiled   = named "tiled" $ spacing 3 $ Tall 1 (5/100) (2/(1+(toRational(sqrt 5 ::Double))))
        -- Split horizonatlly in the same way
        mtiled  = named "mtiled" $ Mirror tiled




{- WORKSPACES -}

alphabet :: [Char]
alphabet =  'a':'b':'c':'d':'e':{-'f':-}'g':'h':'i':'j':'k':'l':'m':'n':'o':'p':'q':'r':'s':'t':'u':'v':'w':'x':{-'y':-}'z':[]

workspacePrefix :: String
workspacePrefix = "w_"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map (\x -> workspacePrefix ++ [x]) alphabet

workspaceKeys :: [KeySym]
workspaceKeys = [ xK_a, xK_b, xK_c, xK_d, xK_e, {- xK_f,-} xK_g, xK_h, xK_i, xK_j, xK_k, xK_l, xK_m, xK_n, xK_o, xK_p, xK_q, xK_r, xK_s, xK_t, xK_u, xK_v, xK_w, xK_x, {- xK_y,-} xK_z ]

workspaceMapping :: [(WorkspaceId, KeySym)]
workspaceMapping = zip myWorkspaces workspaceKeys

-- The workspace that will bee on screen after launch
startupWorkspace = workspacePrefix ++ ['a']



{- APPLICATIONS -}

-- Restart Xmonad
restartXmonad       :: X ()
restartXmonad       = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- System States
suspend, shutdown   :: X ()
suspend             = spawn "pm-suspend"
shutdown            = spawn "shutdown now"

logOut              :: X ()
logOut              = io exitSuccess

-- Terminals
term                :: String
term                = "urxvt"

spawnTerminal       :: X ()
spawnTerminal       = spawn term

terminalWTitle          :: String -> String
terminalWTitle title    = term ++ "-title " ++ title ++ " "

-- Editors
editor              = "emacsclient -c"
editor2             = "vim"

-- Workflow TODO
workflow            = editor ++ " /home/syd/workflow/workflow.txt"

-- Dmenu with custom settings
dmenu               :: X ()
dmenu               = spawn $ "dmenu_run -b -i -l 5 -nb '" ++ "#000000" ++ "' -nf '" ++ colorSecondary ++ "' -sb '" ++ "#000000" ++ "' -sf '" ++ colorMain ++ "'"

-- Internet application
internet_classes    = ["Firefox"]

internet            :: X ()
internet            = spawn "firefox"

-- Mail application
mailClasses        = ["mutt"]
mail                = "urxvt -e zsh -c \"mutt\""

-- Files application
files               :: X ()
files               = spawn "nautilus --no-desktop"
    
-- Scanner
scanner             :: X ()
scanner             = spawn "scangearmp"

-- Brightness
lightDown, lightUp  :: X ()
lightDown           = spawn "xbacklight -dec 10 -steps 1" 
lightUp             = spawn "xbacklight -inc 10 -steps 1"

-- Volume
mute                :: X ()
mute                = spawn "amixer -q set Master 0%"

volumeDown,volumeUp :: X ()
volumeDown          = spawn "amixer -q set Master 4%-"
volumeUp            = spawn "amixer -q set Master 4%+"

-- to define placeholders
nothing             :: X ()
nothing             = return ()


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
        , ((0, xK_F5), spawn "xmodmap ~/.keyboards/dvorak.quintus")
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
        ((myModMask                                 , xK_braceleft      ),  nothing                 ), 
        ((myModMask                                 , xK_plus           ),  lessWindows             ), 
        ((myModMask                                 , xK_parenleft      ),  focusWindowDown         ), 
        ((myModMask .|. shiftMask                   , xK_parenleft      ),  swapWindowDown          ), 
        ((myModMask                                 , xK_equal          ),  shrinkWindow            ), 
        ((myModMask                                 , xK_Insert         ),  selectedSearch          ), 
        ((myModMask                                 , xK_asterisk       ),  expandWindow            ), 
        ((myModMask                                 , xK_parenright     ),  focusWindowUp           ), 
        ((myModMask .|. shiftMask                   , xK_parenright     ),  swapWindowUp            ), 
        ((myModMask                                 , xK_exclam         ),  moreWindows             ), 
        ((myModMask                                 , xK_braceright     ),  nothing                 ), 
        ((myModMask                                 , xK_bracketright   ),  nothing                 ), 


        ((myModMask                                 , xK_Return ),  promptedSearch                  ), 
        ((myModMask .|. shiftMask                   , xK_Return ),  changeKeyboardLayout            ),
        ((myModMask .|. controlMask                 , xK_Return ),  logOut                          ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Return ),  nothing                         ),

        ((myModMask                                 , xK_space  ),  nextLayout                      ),
        ((myModMask .|. shiftMask                   , xK_space  ),  nothing                         ), 
        ((myModMask .|. controlMask                 , xK_space  ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_space  ),  nothing                         ),

        ((myModMask                                 , xK_Tab    ),  nextWindow                      ), 
        ((myModMask .|. shiftMask                   , xK_Tab    ),  previousWindow                  ),
        ((myModMask .|. controlMask                 , xK_Tab    ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Tab    ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_a      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_a      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_b      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_b      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_c      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_c      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_d      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_d      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_e      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_e      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_f      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_f      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_g      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_g      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_h      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_h      ),  nothing                         ),

        ((myModMask .|. controlMask                 , xK_i      ),  nothing                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_i      ),  nothing                         ),
    				
        ((myModMask .|. controlMask                 , xK_j      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_j      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_k      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_k      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_l      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_l      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_m      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_m      ),  nothing                       ),
                
        ((myModMask .|. controlMask                 , xK_n      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_n      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_o      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_o      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_p      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_p      ),  nothing                       ),
                
        ((myModMask .|. controlMask                 , xK_q      ),  suspend                         ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_q      ),  shutdown                        ),

        ((myModMask .|. controlMask                 , xK_r      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_r      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_s      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_s      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_t      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_t      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_u      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_u      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_v      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_v      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_w      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_w      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_x      ),  shellPrompt myXPConfig        ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_x      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_y      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_y      ),  nothing                       ),

        ((myModMask .|. controlMask                 , xK_z      ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_z      ),  nothing                       ),

        ((myModMask                                 , xK_comma  ),  spawn workflow                ),
        ((myModMask .|. shiftMask                   , xK_comma  ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_comma  ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_comma  ),  nothing                       ),

        ((myModMask                                 , xK_period ),  internet                      ),
        ((myModMask .|. shiftMask                   , xK_period ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_period ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_period ),  nothing                       ),

        ((myModMask                                 , xK_F1     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F1     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F1     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F1     ),  nothing                       ),

        ((myModMask                                 , xK_F2     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F2     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F2     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F2     ),  nothing                       ),

        ((myModMask                                 , xK_F3     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F3     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F3     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F3     ),  nothing                       ),

        ((myModMask                                 , xK_F4     ),  spawn mail                    ),
        ((myModMask .|. shiftMask                   , xK_F4     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F4     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F4     ),  nothing                       ),
       
        ((myModMask                                 , xK_F5     ),  lightDown                     ),
        ((myModMask .|. shiftMask                   , xK_F5     ),  nothing                       ),    
        ((myModMask .|. controlMask                 , xK_F5     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F5     ),  nothing                       ),

        ((myModMask                                 , xK_F6     ),  lightUp                       ),
        ((myModMask .|. shiftMask                   , xK_F6     ),  nothing                       ), 
        ((myModMask .|. controlMask                 , xK_F6     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F6     ),  nothing                       ),
 
        ((myModMask                                 , xK_F7     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F7     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F7     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F7     ),  nothing                       ),

        ((myModMask                                 , xK_F8     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F8     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F8     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F8     ),  nothing                       ),

        ((myModMask                                 , xK_F9     ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_F9     ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F9     ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F9     ),  nothing                       ),

        ((myModMask                                 , xK_F10    ),  mute                          ),
        ((myModMask .|. shiftMask                   , xK_F10    ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_F10    ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F10    ),  nothing                       ),
       
        ((myModMask                                 , xK_F11    ),  volumeDown                    ),
        ((myModMask .|. shiftMask                   , xK_F11    ),  lightDown                     ),
        ((myModMask .|. controlMask                 , xK_F11    ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F11    ),  nothing                       ),
        
        ((myModMask                                 , xK_F12    ),  volumeUp                      ),
        ((myModMask .|. shiftMask                   , xK_F12    ),  lightUp                       ),
        ((myModMask .|. controlMask                 , xK_F12    ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_F12    ),  nothing                       ),
       
        ((myModMask                                 , xK_plus   ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_plus   ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_plus   ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_plus   ),  nothing                       ),
        
        ((myModMask                                 , xK_minus  ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_minus  ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_minus  ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_minus  ),  nothing                       ),
        
        ((myModMask                                 , xK_Delete ),  nothing                       ),
        ((myModMask .|. shiftMask                   , xK_Delete ),  nothing                       ),
        ((myModMask .|. controlMask                 , xK_Delete ),  nothing                       ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_Delete ),  nothing                       ),
        
        ((myModMask                                 , xK_BackSpace  ),  tileAgain                 ),
        ((myModMask .|. shiftMask                   , xK_BackSpace  ),  nothing                   ),
        ((myModMask .|. controlMask                 , xK_BackSpace  ),  nothing                   ),
        ((myModMask .|. controlMask .|. shiftMask   , xK_BackSpace  ),  nothing                   )
    ]
    ++
    navigationKeys
    
-- Switch to the letter of a workspace by pressing mod + letter
navigationKeys =
    [
        ((mask .|. myModMask, key), windows $ switch workspace)
        | (workspace, key) <- zip myWorkspaces workspaceKeys
        , (switch, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]
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
        ---- Scroll_down          Nothing
        --((myModMask                                 , button4   ), nothing                                                    ),
        ---- Scroll_up            Nothing
        --((myModMask                                 , button5   ), nothing                                                    )
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
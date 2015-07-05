module Keys
    (
    module Keys,
    module Modifier
    )
    where

import qualified Data.Map                 as M

-- X Keys
import           Graphics.X11.Types

import           XMonad                   (X, XConfig (..))
import           XMonad                   ((.|.))
import           XMonad                   (ChangeLayout (..), IncMasterN (..),
                                           Layout (..), Resize (..), focus, io,
                                           kill, refresh, sendMessage, spawn,
                                           windows, withFocused)
import           XMonad                   (mouseMoveWindow, mouseResizeWindow)
import           XMonad                   (setLayout)
import           XMonad.Actions.Plane     (Limits (Finite), Lines (..),
                                           planeKeys)
import qualified XMonad.Actions.Search    as S
import qualified XMonad.Actions.Submap    as SM
import           XMonad.Hooks.UrgencyHook (focusUrgent)
import           XMonad.Prompt            (XPConfig (..), defaultXPConfig)
import qualified XMonad.StackSet          as W

import           System.Exit              (exitSuccess)

import           XMonad.Prompt.Shell      (shellPrompt)

import           Prelude                  hiding (mod)

import           Actions
import           Modifier
import           Workspaces


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*", height=22}

searchEntered :: X ()
searchEntered = SM.submap (searchEngineMap $ S.promptSearch defaultXPConfig)

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
        ((mod .|. shiftMask                   , xK_Return ),  spawnTerminal                       ) -- Spawn my terminal.
    ,   ((mod .|. controlMask .|. shiftMask   , xK_Return ),  SM.submap keyboardMap               )
    ,   ((mod                                 , xK_space  ),  sendMessage NextLayout              ) -- Select the next layout.
    ,   ((mod .|. shiftMask                   , xK_space  ),  setLayout $ layoutHook conf  ) -- Select the first layout.
    ,   ((mod                                 , xK_Tab    ),  windows W.focusDown                 ) -- Select the next window.
    ,   ((mod .|. shiftMask                   , xK_Tab    ),  windows W.focusUp                   ) -- Select the previous window.
    ,   ((mod .|. shiftMask                   , xK_c      ),  kill                                ) -- Close the selected window.
    ,   ((mod                                 , xK_d      ),  dmenu                               ) -- Show my dmenu.
    ,   ((mod                                 , xK_f      ),  files                               ) -- Open my file explorer.
    ,   ((mod                                 , xK_h      ),  sendMessage Shrink                  ) -- Shrink the master window.
    ,   ((mod                                 , xK_i      ),  internet                            ) -- Open my internet browser.
    ,   ((mod                                 , xK_j      ),  windows W.focusDown                 ) -- Select the previous window.
    ,   ((mod .|. shiftMask                  , xK_j       ),  windows W.swapDown                  ) -- Swap the selected window with the previous window.
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
    ,   ((mod                                 , xK_x      ),  screenshot                          ) -- Take a screenshot
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
myMouse (XConfig {modMask = m}) = M.fromList
    [
        -- Left_mouse_button    Set the window to floating mode and move by dragging
        ((m , button1   ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster      )
        -- Right_mouse_button   Raise the window to the top of the stack
    ,   ((m , button2   ), windows . (W.shiftMaster .) . W.focusWindow                      )
        -- Middle_mouse_button  Set the window to floating mode and resize by dragging
    ,   ((m , button3   ), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster    )
    ]


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

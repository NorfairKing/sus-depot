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
import           XMonad                   (Layout (..), focus, refresh, spawn,
                                           windows)
import           XMonad                   (mouseMoveWindow, mouseResizeWindow)
import           XMonad                   (setLayout)
import           XMonad.Actions.Plane     (Limits (Finite), Lines (..),
                                           planeKeys)
import qualified XMonad.Actions.Search    as S
import qualified XMonad.Actions.Submap    as SM
import           XMonad.Hooks.UrgencyHook (focusUrgent)
import qualified XMonad.StackSet          as W


import           XMonad.Prompt.Shell      (shellPrompt)

import           Prelude                  hiding (mod)

import           Actions
import           Modifier
import           Workspaces


searchEntered :: X ()
searchEntered = SM.submap (searchEngineMap $ S.promptSearch myXPConfig)

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
        ((mod .|. shiftMask                   , xK_Return ),  spawnTerminal                 )
    ,   ((mod .|. controlMask .|. shiftMask   , xK_Return ),  SM.submap keyboardMap         )
    ,   ((mod                                 , xK_space  ),  nextLayout                    )
    ,   ((mod .|. shiftMask                   , xK_space  ),  setLayout $ layoutHook conf   )
    ,   ((mod                                 , xK_Tab    ),  focusWindowDown               )
    ,   ((mod .|. shiftMask                   , xK_Tab    ),  focusWindowUp                 )
    ,   ((mod .|. shiftMask                   , xK_c      ),  closeWindow                   )
    ,   ((mod                                 , xK_d      ),  dmenu                         )
    ,   ((mod                                 , xK_f      ),  files                         )
    ,   ((mod                                 , xK_h      ),  shrinkWindow                  )
    ,   ((mod                                 , xK_i      ),  internet                      )
    ,   ((mod                                 , xK_j      ),  focusWindowDown               )
    ,   ((mod .|. shiftMask                   , xK_j      ),  swapWindowDown                )
    ,   ((mod                                 , xK_l      ),  expandWindow                  )
    ,   ((mod                                 , xK_m      ),  focusMaster                   )
    ,   ((mod .|. shiftMask                   , xK_m      ),  swapMaster                    )
    ,   ((mod                                 , xK_n      ),  refresh                       )
    ,   ((mod                                 , xK_q      ),  restart_xmonad                )
    ,   ((mod .|. shiftMask                   , xK_q      ),  logOut                        )
    ,   ((mod .|. controlMask                 , xK_q      ),  suspend                       )
    ,   ((mod .|. controlMask .|. shiftMask   , xK_q      ),  shutdown                      )
    ,   ((mod                                 , xK_s      ),  searchEntered                 )
    ,   ((mod .|. shiftMask                   , xK_s      ),  searchSelected                )
    ,   ((mod                                 , xK_t      ),  tileAgain                     )
    ,   ((mod                                 , xK_u      ),  focusUrgent                   )
    ,   ((mod                                 , xK_x      ),  screenshot                    )
    ,   ((mod .|. controlMask                 , xK_x      ),  shellPrompt myXPConfig        )
    ,   ((mod                                 , xK_comma  ),  moreWindows                   )
    ,   ((mod .|. shiftMask                   , xK_comma  ),  lessWindows                   )
    ,   ((mod                                 , xK_F4     ),  mail                          )
    ,   ((mod                                 , xK_F5     ),  lightDown                     )
    ,   ((mod                                 , xK_F6     ),  lightUp                       )
    ,   ((mod                                 , xK_F10    ),  mute                          )
    ,   ((mod                                 , xK_F11    ),  volumeDown                    )
    ,   ((mod                                 , xK_F12    ),  volumeUp                      )
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

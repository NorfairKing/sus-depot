module Keys
    ( module Keys
    , module Modifier
    ) where


import qualified Data.Map              as M

-- X Keys
import           Graphics.X11.Types

import           XMonad                (Layout (..), X, XConfig (..), focus,
                                        mouseMoveWindow, mouseResizeWindow,
                                        spawn, windows, (.|.))
import           XMonad.Actions.Plane  (Limits (Finite), Lines (..), planeKeys)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet       as W




import           XMonad.Prompt.Shell   (shellPrompt)

import           Prelude               hiding (mod)

import           Actions
import           Modifier
import           Workspaces


-- Perform search on prompted input.
promptedSearch      :: X ()
promptedSearch      = SM.submap (searchEngineMap $ S.promptSearch myXPConfig)

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

{- KEYBINDINGS -}
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys _ = M.fromList $
    [
        ((mod                                 , xK_f            ),  spawnTerminal       ),
        ((mod                                 , xK_d            ),  closeWindow         ),


        ((mod                                 , xK_o            ),  focusWindowDown     ),
        ((mod .|. shiftMask                   , xK_o            ),  swapWindowDown      ),
        ((mod                                 , xK_e            ),  focusWindowUp       ),
        ((mod .|. shiftMask                   , xK_e            ),  swapWindowUp        ),

        ((mod                                 , xK_comma        ),  shrinkWindow        ),
        ((mod                                 , xK_period       ),  expandWindow        ),

        ((mod                                 , xK_q            ),  lessWindows         ),
        ((mod                                 , xK_j            ),  moreWindows         ),

        ((mod                                 , xK_space        ),  nextLayout          ),
        ((mod                                 , xK_Tab          ),  nextWindow          ),
        ((mod .|. shiftMask                   , xK_Tab          ),  previousWindow      ),
        ((mod                                 , xK_b            ),  internet            ),

        ((mod                                 , xK_F5           ),  lightDown           ),
        ((mod                                 , xK_F6           ),  lightUp             ),
        ((mod                                 , xK_F10          ),  mute                ),
        ((mod                                 , xK_F11          ),  volumeDown          ),
        ((mod .|. shiftMask                   , xK_F11          ),  lightDown           ),
        ((mod                                 , xK_F12          ),  volumeUp            ),
        ((mod .|. shiftMask                   , xK_F12          ),  lightUp             ),
        ((mod                                 , xK_BackSpace    ),  tileAgain           )
    ]
    ++
    navigationKeys

-- Switch to the letter of a workspace by pressing mod + letter
navigationKeys :: [((KeyMask, KeySym), X ())]
navigationKeys =
    [
        (
            (mask .|. mod, key)
            ,
            windows $ switch ws
        )
        |
        (ws, key) <- zip myWorkspaces workspaceKeys,
        (switch, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ] ++
    M.toList (planeKeys mod (Lines 4) Finite)



-- | Mouse bindings: default actions bound to mouse events
myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse _ = M.fromList
    [
        -- Left_mouse_button    Set the window to floating mode and mov by dragging
        ((mod                                 , button1   ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster      ),
        -- Middle_mouse_button  Do a fancy search
        ((mod                                 , button2   ), \_ -> selectedSearch                                             ),
        -- Right_mouse_button   Set the window to floating mode and resize by dragging
        ((mod                                 , button3   ), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)  )
    ]


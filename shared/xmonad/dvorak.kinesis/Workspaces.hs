module Workspaces where

import           Graphics.X11.Types
import           XMonad             (WorkspaceId)

alphabet :: [Char]
alphabet =
    [
                             'p', {-'y',-} {-'f',-} 'g',   'c',   'r',   'l',
        'a',   'o',   'e',   'u',   'i',     'd',   'h',   't',   'n',   's',
               'q',   'j',   'k',   'x',     'b',   'm',   'w',   'v',   'z'
    ]

-- | Where to send applications
workflow_ws = workspace 'a'
mail_ws     = workspace 'z'
web_ws      = workspace 'i'

workspacePrefix :: String
workspacePrefix = "w_"

workspace :: Char -> WorkspaceId
workspace s = workspacePrefix ++ [s]

myWorkspaces :: [WorkspaceId]
myWorkspaces = map (\x -> workspace [x]) alphabet

workspaceKeys :: [KeySym]
workspaceKeys =
    [
                                xK_p, {-xK_y,-} {-xK_f,-} xK_g,   xK_c,   xK_r,   xK_l,
        xK_a,   xK_o,   xK_e,   xK_u,   xK_i,     xK_d,   xK_h,   xK_t,   xK_n,   xK_s,
                xK_q,   xK_j,   xK_k,   xK_x,     xK_b,   xK_m,   xK_w,   xK_v,   xK_z
    ]

workspaceMapping :: [(WorkspaceId, KeySym)]
workspaceMapping = zip myWorkspaces workspaceKeys

-- The workspace that will bee on screen after launch
startupWorkspace = workspace 'i'

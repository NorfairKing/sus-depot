module Workspaces where

import           Graphics.X11.Types
import           XMonad             (WorkspaceId)

workspaceLetters :: [Char]
workspaceLetters =
    [ 'g', 'c', 'r'
    , 'h', 't', 'n'
    , 'm', 'v', 'w'
    ]

-- | Where to send applications
workflow_ws = workspace 'm'
mail_ws     = workspace 'v'
web_ws      = workspace 'n'

workspace :: Char -> WorkspaceId
workspace s = [s]

myWorkspaces :: [WorkspaceId]
myWorkspaces = map workspace workspaceLetters

workspaceKeys :: [KeySym]
workspaceKeys =
    [ xK_g, xK_c, xK_r
    , xK_h, xK_t, xK_n
    , xK_m, xK_v, xK_w
    ]

workspaceMapping :: [(WorkspaceId, KeySym)]
workspaceMapping = zip myWorkspaces workspaceKeys

-- The workspace that will bee on screen after launch
startupWorkspace = workspace 't'

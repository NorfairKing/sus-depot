module Workspaces where

import XMonad (WorkspaceId, KeySym)
import Graphics.X11.Types -- For key identifiers like xK_a .. xK_z

alphabet :: [Char]
alphabet =  'a':'b':'c':'d':'e':{-'f':-}'g':'h':'i':'j':'k':'l':'m':'n':'o':'p':'q':'r':'s':'t':'u':'v':'w':'x':{-'y':-}'z':[]

workspacePrefix :: String
workspacePrefix = "w_"

workspace :: String -> WorkspaceId
workspace s = workspacePrefix ++ s

myWorkspaces :: [WorkspaceId]
myWorkspaces = map (\x -> workspace [x]) alphabet

workspaceKeys :: [KeySym]
workspaceKeys = [ xK_a, xK_b, xK_c, xK_d, xK_e, {- xK_f,-} xK_g, xK_h, xK_i, xK_j, xK_k, xK_l, xK_m, xK_n, xK_o, xK_p, xK_q, xK_r, xK_s, xK_t, xK_u, xK_v, xK_w, xK_x, {- xK_y,-} xK_z ]

workspaceMapping :: [(WorkspaceId, KeySym)]
workspaceMapping = zip myWorkspaces workspaceKeys

-- The workspace that will bee on screen after launch
startupWorkspace = workspace ['a']

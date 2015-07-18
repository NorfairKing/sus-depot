module Workspaces where

import           XMonad (WorkspaceId)

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

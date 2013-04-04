import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Actions.CycleWS      -- workspace-switching
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)
import XMonad.Config.Kde
import XMonad.Layout
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Layout.Spacing  
import System.IO

myManageHook = composeAll
  [ className =? "yakuake" --> doFloat
  , className =? "Yakuake" --> doFloat
  , className =? "plasma" --> doFloat
  , className =? "Plasma" --> doFloat
  ]

myLayout = tiled ||| Mirror tiled ||| Full
    where
    -- default tiling algorithm partitions the --screen into two panes  
    tiled = spacing 5 $ Tall nmaster delta ratio
    nmaster = 1 -- The default number of windows in the master pane 
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 5/100  -- % of screen to increment by when resizing panes
  
  
main = do
  xmonad $ desktopConfig 
    { modMask = mod4Mask
    , manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , borderWidth = 1
    , normalBorderColor = "#abc123"
    , focusedBorderColor = "#456def"
    , layoutHook = avoidStruts myLayout
    }
    `additionalKeysP` myKeys

myKeys = [ ("M-<Tab>"    , toggleWS                  ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                    ) -- go to next workspace
         , ("M-b"        , sendMessage ToggleStruts  ) -- toggle the status bar gap
         , ("M-<Left>"   , prevWS                    ) -- go to prev workspace
         ]

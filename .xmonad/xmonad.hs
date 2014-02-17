import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Actions.CycleWS      -- workspace-switching
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)
import XMonad.Config.Xfce
import XMonad.Layout
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Layout.Spacing  
import System.IO

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.

myManageHook = composeAll
  [ className =? "xfdesktop"      --> doFloat
  , className =? "Xfdesktop"      --> doFloat
  , className =? "xfce4-panel"    --> doFloat
  , className =? "Xfce4-panel"    --> doFloat
  , className =? "xfdesktop"      --> doIgnore
  , className =? "Xfdesktop"      --> doIgnore
  , className =? "stalonetray"    --> doIgnore
  , className =? "VirtualBox"     --> doShift "6:vm"
  , className =? "steam"          --> doShift "5:media"
  , className =? "Steam"          --> doShift "5:media"
  , className =? "pidgin"         --> doShift "4:im"
  , className =? "Pidgin"         --> doShift "4:im"
  , className =? "Thunderbird"    --> doShift "3:mail"
  , className =? "Mail"           --> doShift "3:mail"
  , className =? "vlc"            --> doShift "7:media"
  , className =? "Vlc"            --> doShift "7:media"
  , className =? "kaffiene"       --> doShift "7:media"
  ]

myLayout = tiled ||| Mirror tiled ||| Full
    where
    -- default tiling algorithm partitions the 
    --screen into two panes  
    tiled = spacing 5 $ Tall nmaster delta ratio
    nmaster = 1 -- The default number of windows in the master pane 
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 5/100  -- % of screen to increment by when resizing panes
  
myWorkspaces = ["1:grnd0","2:web","3:mail","4:im","5:", "6:vm","7:media","8:games","9:steam"]

logHook' xmproc =
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 80
    }

main = do
  xmproc <- spawnPipe "killall xmobar ; xmobar ~/.xmonad/xmobar.hs"
  traybar <- spawnPipe "killall stalonetray ; stalonetray -i 20 --max-geometry 8x1-0+0 --icon-gravity E --geometry 8x1-0+0 -bg '#'242424 --sticky --skip-taskbar"
  xmonad $ ewmh desktopConfig 
    { modMask = mod4Mask
    , terminal = "xfce4-terminal"
    , manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , borderWidth = 1
    , normalBorderColor = "#abc123"
    , focusedBorderColor = "#456def"
    , layoutHook = smartBorders . avoidStruts $ myLayout
    , workspaces = myWorkspaces
    , startupHook = startupHook desktopConfig
    , logHook     = logHook' xmproc
    }
    `additionalKeysP` myKeys

myKeys = [ ("M-<Tab>"    , toggleWS                  ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                    ) -- go to next workspace
         , ("M-b"        , sendMessage ToggleStruts  ) -- toggle the status bar gap
         , ("M-<Left>"   , prevWS                    ) -- go to prev workspace
         ]

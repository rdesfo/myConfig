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

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.

myManageHook = composeAll
  [ className =? "yakuake" --> doFloat
  , className =? "Yakuake" --> doFloat
  , className =? "Kmix" --> doFloat
  , className =? "kmix" --> doFloat
  , className =? "plasma" --> doFloat
  , className =? "Plasma" --> doFloat
  , className =? "plasma-desktop" --> doFloat
  , className =? "Plasma-desktop" --> doFloat
  , className =? "krunner" --> doFloat
  , className =? "ksplashsimple" --> doFloat
  , className =? "ksplashqml" --> doFloat
  , className =? "ksplashx" --> doFloat
  , resource  =? "Games"         --> doShift "6:game"
  , className =? "VirtualBox"    --> doShift "4:vm"
  , className =? "steam"         --> doShift "5:media"
  , className =? "stalonetray"   --> doIgnore
  ]

myLayout = tiled ||| Mirror tiled ||| Full
    where
    -- default tiling algorithm partitions the 
    --screen into two panes  
    tiled = spacing 5 $ Tall nmaster delta ratio
    nmaster = 1 -- The default number of windows in the master pane 
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 5/100  -- % of screen to increment by when resizing panes
  
myWorkspaces = ["1:grnd0","2:web","3:mail","4:vm","5:media","6:game"] ++ map show [7..9]

logHook' xmproc = do
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 80
    }

main = do
  xmproc <- spawnPipe "killall xmobar ; xmobar ~/.xmonad/xmobar.hs"
  traybar <- spawnPipe "killall stalonetray ; stalonetray -i 16 --max-geometry 8x1-0+0 --icon-gravity E --geometry 8x1-0+0 -bg black --sticky --skip-taskbar --config $HOME/.xmonad/stalonetrayrc"
  xmonad $ ewmh desktopConfig 
    { modMask = mod4Mask
    , terminal = "konsole"
    , manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , borderWidth = 1
    , normalBorderColor = "#abc123"
    , focusedBorderColor = "#456def"
    , layoutHook = avoidStruts myLayout
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

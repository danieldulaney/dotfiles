module XmonadConfig where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Paste
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.Submap
import XMonad.Actions.OnScreen
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Char
import System.Exit
import System.IO
import Solarized

main = do 
    xmonad =<< statusBar "xmobar -x 1" myPP (\_ -> (0, 0)) (ewmh myConfig)

myLayout = windowNavigation tiled ||| Full
    where
        tiled = spacingRaw True (Border 5 5 5 5) False (Border 5 5 5 5) True $ Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

highlight = xmobarColor solarizedBackground

wsSwitch ws = xmobarAction ("wmctrl -s " ++ ws) "1" ws

myPP :: PP
myPP = def
    { ppCurrent = highlight solarizedBlue . pad
    , ppVisible = highlight solarizedViolet . wsSwitch . pad
    , ppHidden = highlight solarizedBase01 . wsSwitch . pad
    , ppHiddenNoWindows = const ""
    , ppUrgent = highlight solarizedYellow . pad
    , ppSep = " ┃ "
    , ppTitle = id
    , ppTitleSanitize = xmobarStrip
    , ppLayout = id
    , ppSort = getSortByXineramaRule
    }

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

-- Generate keybindings
-- Action is Go or Swap
windowMovementKeys mod keys action dirs =
    [(( mod , key ), sendMessage $ action dir)
        | (key, dir) <- zip keys dirs]

screenLock = "i3lock -f -c " ++ show solarizedBase01

myKeys conf = M.fromList $
    [ ((mod4Mask   , xK_l), spawn screenLock)
    , ((mod4Mask   , xK_r), spawn "dmenu_run")
    , ((mod1Mask   , xK_Tab), windows W.focusDown)
    , ((controlMask, xK_a), submap . M.fromList $
            [ ((controlMask, xK_a     ), sendKey controlMask xK_a)
            , ((0,           xK_q     ), spawn "xmonad --recompile && xmonad --restart")
            , ((shiftMask,   xK_q     ), io (exitWith ExitSuccess))
            , ((0,           xK_r     ), spawn "dmenu_run")
            , ((shiftMask,   xK_Return), spawn $ XMonad.terminal conf)
            , ((0,           xK_space ), sendMessage NextLayout)
            , ((0,           xK_Return), windows W.swapMaster)
            , ((0,           xK_w     ), viewBrowser)
            , ((0,           xK_e     ), viewEmail)
            , ((0,           xK_s     ), viewScreen 0)
            , ((0,           xK_d     ), viewScreen 1)
            , ((0,           xK_f     ), viewScreen 2)
            , ((0,           xK_c     ), kill)
            , ((shiftMask,   xK_c     ), killAll)
            ]

            -- hjkl to move between windows
            ++ windowMovementKeys 0 [xK_h, xK_j, xK_k, xK_l] Go [L, D, U, R]

            -- HJKL to swap window positions
            ++ windowMovementKeys shiftMask [xK_h, xK_j, xK_k, xK_l] Swap [L, D, U, R]

            -- 0-9 to select current workspace
            ++ [((0, key), windows (W.greedyView wkspace))
                    | (key, wkspace) <- zip [xK_1..xK_9] (map show [1..9])]

            -- 0-9 to send to workspace
            ++ [((shiftMask, key), windows (W.shift wkspace))
                    | (key, wkspace) <- zip [xK_1..xK_9] (map show [1..9])]
      )
    ]
    ++ windowMovementKeys mod4Mask [xK_Right, xK_Left, xK_Up, xK_Down] Swap [R, L, U, D]

viewScreen :: ScreenId -> X ()
viewScreen sc = screenWorkspace sc >>= flip whenJust (windows . W.view)

-- Show the given workspace and show or launch a program
viewProgram :: String -> ScreenId -> String -> String -> X ()
viewProgram workspace screen windowClass command = do
    { windows (greedyViewOnScreen screen workspace)
    ; runOrRaise command (className =? windowClass)
    }

snapProgram workspace screen windowClass = className =? windowClass -->
    (doShift workspace) <+> (doF $ greedyViewOnScreen screen workspace)

viewBrowser = viewProgram "web" 0 "Firefox Developer Edition" "firefox-developer-edition"
snapBrowser = snapProgram "web" 0 "Firefox Developer Edition"

viewEmail = viewProgram "email" 0 "Thunderbird" "thunderbird"
snapEmail = viewProgram "email" 0 "Thunderbird"

myWorkspaces = ["web", "email"] ++ map show [1..9]

myManageHook = composeAll
    [ snapBrowser
    ]

--myManageHook = composeAll
--    [ className =? "Firefox Developer Edition" -->
--        (doShift "web") <+> (doF $ W.greedyView "web")
--    ]

myConfig = def
        { borderWidth = 1
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , normalBorderColor = solarizedBase02
        , focusedBorderColor = solarizedBlue
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , logHook = myLogHook
        , manageHook = myManageHook <+> manageHook def
        , startupHook = viewEmail >> viewBrowser 
        , handleEventHook = handleEventHook def
        , XMonad.keys = myKeys
        , XMonad.modMask = mod4Mask
        }

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Paste
import XMonad.Actions.Submap
import XMonad.Actions.OnScreen
import XMonad.Actions.WindowGo
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

myPP :: PP
myPP = def
    { ppCurrent = highlight solarizedBlue . pad
    , ppVisible = highlight solarizedViolet . pad
    , ppHidden = highlight solarizedBase01 . pad
    , ppHiddenNoWindows = const ""
    , ppUrgent = highlight solarizedYellow . pad
    , ppSep = " ┃ "
    , ppTitle = id
    , ppTitleSanitize = xmobarStrip
    , ppLayout = id
    }

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

-- Generate keybindings
-- Action is Go or Swap
windowMovementKeys mod keys action dirs =
    [(( mod , key ), sendMessage $ action dir)
        | (key, dir) <- zip keys dirs]

myKeys conf = M.fromList $
    [ ((mod4Mask   , xK_l), spawn "physlock")
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
            , ((0,           xK_s     ), viewScreen 0)
            , ((0,           xK_d     ), viewScreen 1)
            , ((0,           xK_f     ), viewScreen 2)
            , ((0,           xK_c     ), kill)
            ]

            -- hjkl to move between windows
            ++ windowMovementKeys 0 [xK_h, xK_j, xK_k, xK_l] Go [L, D, U, R]

            -- HJKL to swap window positions
            ++ windowMovementKeys shiftMask [xK_h, xK_j, xK_k, xK_l] Swap [L, D, U, R]

            -- 0-9 to select current workspace
            ++ [((0, key), windows (W.greedyView wkspace))
                    | (key, wkspace) <- zip [xK_1..xK_9] (map show [1..9])]

            -- Shift + 0-9 to send to workspace
            ++ [((shiftMask, key), spawn "xmessage hi")
                    | (key, wkspace) <- zip [xK_1..xK_9] (map show [1..9])]
      )
    ]
    ++ windowMovementKeys mod4Mask [xK_Right, xK_Left, xK_Up, xK_Down] Swap [R, L, U, D]

viewScreen :: ScreenId -> X ()
viewScreen sc = screenWorkspace sc >>= flip whenJust (windows . W.view)

browserWorkspace = "web"
browserScreen = 0
browserCommand = "firefox-developer-edition"
browserClass = "Firefox Developer Edition"

-- Show the given workspace and show or launch a browser
viewBrowser :: X ()
viewBrowser = do
    { windows (greedyViewOnScreen browserScreen browserWorkspace)
    ; runOrRaise browserCommand (className =? browserClass)
    }

myWorkspaces = ["web"] ++ map show [1..9]

myManageHook = composeAll
    [ className =? browserClass -->
        (doShift "web") <+> (doF $ W.greedyView browserWorkspace)
    ]

solarized_base02 = "#073642"
solarized_blue = "#268bd2"

myConfig = def
        { borderWidth = 1
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , normalBorderColor = solarized_base02
        , focusedBorderColor = solarized_blue
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , logHook = myLogHook
        , manageHook = myManageHook <+> manageHook def
        , startupHook = viewBrowser
        , handleEventHook = handleEventHook def
        , XMonad.keys = myKeys
        , XMonad.modMask = mod4Mask
        }
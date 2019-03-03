import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Util.Paste
import XMonad.Actions.Submap
import XMonad.Layout.Spacing
import XMonad.StackSet as W
import Graphics.X11.Xlib.Extras
import Data.Map as M
import Data.Monoid
import Data.Word
import System.Exit

main = do 
    xmonad =<< xmobar myConfig

myLayout = tiled ||| Full
    where
        tiled = smartSpacing 5 $ Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

myKeys _ = M.fromList $
    [ ((mod4Mask, xK_l), spawn "xterm")
    , ((controlMask, xK_a), submap . M.fromList $
            [ ((0, xK_q), spawn "xmonad --recompile; xmonad --restart")
            , ((0, xK_j), windows W.focusDown)
            , ((0, xK_k), windows W.focusUp)
            ])
    ]

solarized_base02 = "#073642"
solarized_blue = "#268bd2"

myConfig = def
        { borderWidth = 1
        , terminal = "xterm"
        , normalBorderColor = solarized_base02
        , focusedBorderColor = solarized_blue
        , layoutHook = myLayout
        , logHook = myLogHook
        , XMonad.keys = myKeys
        , XMonad.modMask = mod4Mask
        }

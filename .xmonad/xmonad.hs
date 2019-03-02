import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Paste
import XMonad.Actions.Submap
import XMonad.Layout.Spacing
import Data.Map as M

main = do 
    xmonad =<< xmobar myConfig

prefixKeymap pfx@(modifier, keycode) customKeys x = M.fromList $
    let defaultKeyMap = XMonad.keys defaultConfig x
        customKeyMap = M.fromList (customKeys x)
        merged = M.union customKeyMap defaultKeyMap
    in
        [ (pfx, submap merged) ]

prefix = (controlMask, xK_a)
customKeys x = 
    [ (prefix, sendKey (fst prefix) (snd prefix))
    ]

myLayout = tiled ||| Full
    where
        tiled = smartSpacing 5 $ Tall nmaster delta ratio
        nmaster = 2
        ratio = 1/2
        delta = 3/100

solarized_base02 = "#073642"
solarized_blue = "#268bd2"

myConfig = def
        { borderWidth = 2
        , terminal = "xterm"
        , normalBorderColor = solarized_base02
        , focusedBorderColor = solarized_blue
        , layoutHook = myLayout
        --, XMonad.keys = prefixKeymap prefix customKeys 
        --, XMonad.modMask = 0
        }

import Xmobar hiding (main)
import Solarized

config :: Config
config = defaultConfig
    { font = "xft:Fira Code:size=12"
    , bgColor = solarizedBackground
    , fgColor = solarizedForeground
    , additionalFonts = [ "xft:Font Awesome 5 Free:style=Solid:size=12" ]
    , border = BottomB
    , borderColor = solarizedBase01
    , position = OnScreen 1 Top
    , commands =
        [ Run $ UnsafeStdinReader
        , Run $ Date "%a %d %b %Y %I:%M:%S %p" "date" 1
        , Run $ Battery
            [ "-t", "<acstatus><watts> <leftvbar> <left>% <timeleft>"
            , "-L", "20", "-H", "80"
            , "--"
            , "-L", "0", "-H", "0"
            , "-O", "P", "-i", "I", "-o", "U"
            ] 10
        , Run $ Memory [] 10
        , Run $ DynNetwork [] 10
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%UnsafeStdinReader% }{ <fn=1>\xf538</fn> %memory% ┃ %battery% ┃ %date%"
    }

main :: IO ()
main = xmobar config

module XmobarConfig where

import Xmobar hiding (main)
import Solarized

import Control.Monad

import Data.List
import Data.List.Split hiding (startsWith)
import qualified Data.Map as M
import Data.Maybe

import Text.Printf
import Text.Read
import Text.Regex.Posix

import FancyMemory
import FancyBattery
import FancyNetwork


-- Main config
--------------
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
        , Run $ FancyMemory 10
        , Run $ FancyBattery 10
        , Run $ FancyNetwork 10
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%UnsafeStdinReader% }{ " ++ "%fancynet% ┃  %fancymem% ┃ %fancybat% ┃ %date%"
    }

main :: IO ()
main = xmobar config

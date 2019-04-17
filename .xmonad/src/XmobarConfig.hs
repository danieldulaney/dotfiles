module XmobarConfig where

import Xmobar hiding (main)
import Solarized

import Data.List.Split

-- Utilities
------------
icon c = "<fn=1>" ++ c : "</fn>"
color fg bg content = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ content ++ "</fc>"
pad s = " " ++ s ++ " "

highlight = color (bgColor config)


-- Plugins
----------


-- FancyMemory plugin
data FancyMemory = FancyMemory 
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyMemory where
    alias (FancyMemory _) = "fancymem"
    run   (FancyMemory _) = show <$> memInfo
    rate  (FancyMemory i) = i

fileMem :: IO String
fileMem = readFile "/proc/meminfo"

-- Returns (total, free)
--memInfo :: IO (Int, Int)
memInfo = (splitOn "\n") <$> fileMem


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
        , Run $ Battery
            [ "-t", "<acstatus><watts> <leftvbar> <left>% <timeleft>"
            , "-L", "20", "-H", "80"
            , "--"
            , "-L", "0", "-H", "0"
            , "-O", "P", "-i", "I", "-o", "U"
            ] 10
        , Run $ Memory [] 10
        , Run $ DynNetwork [] 10
        , Run $ FancyMemory 10
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%UnsafeStdinReader% }{ %fancymem% | %memory% ┃ %battery% ┃ %date%"
    }

main :: IO ()
main = xmobar config

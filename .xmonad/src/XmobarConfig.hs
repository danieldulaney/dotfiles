module XmobarConfig where

import Xmobar hiding (main)
import Solarized

import Data.List
import Data.List.Split
import Data.Maybe

import Text.Regex.Posix

-- Utilities
------------
icon c = "<fn=1>" ++ c : "</fn>"
color fg bg content = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ content ++ "</fc>"
pad s = " " ++ s ++ " "

highlight = color (bgColor config)

toHumanRegex = "[0-9]"
toHuman s = s =~ toHumanRegex


-- Plugins
----------


-- FancyMemory plugin
data FancyMemory = FancyMemory 
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyMemory where
    alias (FancyMemory _) = "fancymem"
    run   (FancyMemory _) = show <$> memItem "MemTotal"
    rate  (FancyMemory i) = i

fileMem :: IO String
fileMem = readFile "/proc/meminfo"

memItem :: String -> IO (Maybe String)
memItem s = listToMaybe <$> (map $ intercalate " ") <$> (map tail) <$> filter (lineMatches s) <$> (map splitFields) <$> splitLines <$> fileMem
    where
        splitLines = endBy "\n"
        splitFields = split (dropBlanks $ dropDelims $ oneOf ": ")
        lineMatches str (first:_) = str == first
        lineMatches _ [] = False
        listItem str = listToMaybe . filter (lineMatches str)

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

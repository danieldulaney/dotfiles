module XmobarConfig where

import Xmobar hiding (main)
import Solarized

import Data.List
import Data.List.Split hiding (startsWith)
import Data.Maybe

import Text.Read
import Text.Regex.Posix

-- Utilities
------------
icon c = "<fn=1>" ++ c : "</fn>"
color fg bg content = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ content ++ "</fc>"
pad s = " " ++ s ++ " "

highlight = color (bgColor config)

byteUnits = [ ('k', 2^10)
            , ('M', 2^20)
            , ('G', 2^30)
            , ('T', 2^40)
            , ('P', 2^50) ]

-- Evaluates the predicate against an item. If it is true, returns the item
-- wrapped in a Maybe. Otherwise, returns Nothing
checkThat :: (a -> Bool) -> a -> Maybe a
checkThat pred item
    | (pred item) = Just item
    | otherwise   = Nothing

fromHuman :: String -> Maybe Integer
fromHuman s = do
    { results <- checkThat (\l -> length l == 2) $ regexResults s
    ; base <- (readMaybe $ head results :: Maybe Integer)
    ; multiplier <- getUnit $ last results
    ; return $ base * multiplier
    } where
        regexResults :: String -> [String]
        regexResults s = mrSubList (s =~ "([[:digit:]]+)[[:blank:]]*([[:alpha:]]*)")
        getUnit :: String -> Maybe Integer
        getUnit unit = return . snd =<< (listToMaybe $ filter (\(u, _) -> u == head unit) byteUnits)

startsWith [] _ = True
startsWith _ [] = False
startsWith (n:ns) (h:hs)
    | n == h    = startsWith ns hs
    | otherwise = False

-- Plugins
----------


-- FancyMemory plugin
data FancyMemory = FancyMemory 
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyMemory where
    alias (FancyMemory _) = "fancymem"
    run   (FancyMemory _) = show <$> memItem "MemFree"
    rate  (FancyMemory i) = i

memFile :: IO String
memFile = readFile "/proc/meminfo"

memItem :: String -> IO (Maybe Integer)
memItem s = do
    { file <- memFile
    ; let lines = splitLines file
    ; let matchinglines = filter (startsWith $ s ++ ":") lines
    ; let matchingvalues = map (getIndex 1 . splitOn ":") matchinglines
    ; let matchingvalue = listToMaybe matchingvalues >>= fromHuman
    ; return $ matchingvalue
    } where
        splitLines = endBy "\n"
        getIndex ix list = list!!ix

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

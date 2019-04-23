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

-- Utilities
------------
icon c = "<fn=1>" ++ c : "</fn>"
color fg bg content = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ content ++ "</fc>"
pad s = " " ++ s ++ " "

highlight = color (bgColor config)

ok = highlight solarizedGreen
warning = highlight solarizedYellow
danger = highlight solarizedRed

alertHighlight :: (Ord a, Fractional a) => a -> (String -> String)
alertHighlight status
    | status < 0.7 = id
    | status < 0.9 = warning
    | otherwise    = danger

alertHighlightPad :: (Ord a, Fractional a) => a -> (String -> String)
alertHighlightPad status string = alertHighlight status (lpad ++ string ++ rpad)
    where (lpad, rpad) = if status < 0.7 then (" ", " ") else ("\x258c", "\x2590")


byteUnits = [ (' ', 2^00)
            , ('k', 2^10)
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

withByteUnits :: Float -> String
withByteUnits v = withUnits (2^10) v ++ "B"

withUnits :: Int -> Float -> String
withUnits = withUnits' 0

withUnits' :: Int -> Int -> Float -> String
withUnits' ix mul val
    | val < 0 = '-' : withUnits' ix mul (-val)
    | val < (fromIntegral mul) || (ix+1 == length units) = (showNum val) ++ " " ++ units!!ix
    | otherwise = withUnits' (ix + 1) mul (val / fromIntegral mul)
    where
        units = ["", "K", "M", "G", "T", "P", "E", "Z", "Y"]
        showSingles val = show (round val :: Int)
        showMults val = printf "%.1g" val :: String
        showNum = if ix == 0 then showSingles else showMults

startsWith [] _ = True
startsWith _ [] = False
startsWith (n:ns) (h:hs)
    | n == h    = startsWith ns hs
    | otherwise = False

divideIntegral lhs rhs = (fromIntegral lhs) / (fromIntegral rhs)

maybePrintf1 :: String -> Maybe Double -> String
maybePrintf1 _ Nothing = "Updating..."
maybePrintf1 fmt (Just item) = printf fmt item

-- FancyBattery plugin
----------------------


-- FancyMemory plugin
---------------------
data FancyMemory = FancyMemory 
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyMemory where
    alias (FancyMemory _) = "fancymem"
    run   (FancyMemory _) = do
        { memInfo <- parseMem
        ; let ratio = memInfo !! 0
        ; let pct = ratio * 100
        ; let string = printf "%-3.1f%%" pct :: String
        ; let used = memInfo !! 8
        ; let total = memInfo !! 3
        ; return $ icon '\xf538' ++ (alertHighlightPad ratio string) ++ "(" ++ (withByteUnits used) ++ "/" ++ (withByteUnits total) ++ ")"
        }
    rate  (FancyMemory i) = i

memFile :: IO String
memFile = readFile "/proc/meminfo"

parseMem :: IO [Float]
parseMem = do 
    { file <- memFile
    ; let content = map words $ take 8 $ lines file
    ; let info = M.fromList $ map (\line -> (head line, (read $ line !! 1 :: Float) * 1024)) content
    ; let [total, free, buffer, cache] = map (info M.!) ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]
    ; let available = M.findWithDefault (free + buffer + cache) "MemAvailable:" info
    ; let used = total - available
    ; let usedratio = used / total
    ; let freeratio = free / total
    ; let availableratio = available / total
    ; return [usedratio, freeratio, availableratio, total, free, buffer, cache, available, used]
    }

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
    , template = "%UnsafeStdinReader% }{ " ++ "%fancymem% ┃ %battery% ┃ %date%"
    }

main :: IO ()
main = xmobar config

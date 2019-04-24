module Utils where

import Solarized
import Text.Printf

icon c = "<fn=1>" ++ c : "</fn>"
color fg bg content = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ content ++ "</fc>"
pad s = " " ++ s ++ " "

-- Use the background color as foreground
highlight = color solarizedBackground

ok = highlight solarizedGreen
warning = highlight solarizedYellow
danger = highlight solarizedRed

alertHighlight :: (Ord a, Fractional a) => a -> (String -> String)
alertHighlight status
    | status < 0.7 = id
    | status < 0.9 = warning
    | otherwise    = danger

alertHighlightLow status
    | status > 0.3 = id
    | status > 0.1 = warning
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

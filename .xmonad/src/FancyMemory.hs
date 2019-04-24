module FancyMemory
    ( FancyMemory (..)
    ) where

import Xmobar hiding (main)
import qualified Data.Map as M
import Text.Printf
import Utils

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

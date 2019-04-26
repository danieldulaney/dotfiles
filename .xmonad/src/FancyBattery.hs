module FancyBattery where

import Xmobar hiding (main, Battery)

import Control.Monad.ST
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.FilePath
import System.Directory
import Text.Printf
import Data.List
import Data.Maybe
import Control.Exception (SomeException, handle)

import Utils

data FancyBattery = FancyBattery
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyBattery where
    alias (FancyBattery _) = "fancybat"
    run   (FancyBattery _) = do
        { powerSupplies <- powerSupplyPaths >>= mapM readPowerSupply
        ; let customIcon = icon $ powerIcon powerSupplies
        ; let detailsSection = intercalate " | " $ catMaybes $ map details powerSupplies
        ; let padding = if not $ null detailsSection then " " else ""
        ; return $ customIcon ++ padding ++ detailsSection
        }
    rate  (FancyBattery i) = i

data Files = Files
    { fFull :: String
    , fNow :: String
    , fVoltage :: String
    , fCurrent :: String
    , fStatus :: String
    , isCurrent :: Bool
    } | NoFiles deriving (Eq, Show)

data PowerSupply =
    Battery
        { full :: !Float
        , now :: !Float
        , power :: !Float
        , status :: !String
        }
    | Ac { online :: !Bool }
    | Usb 
    | Bad { info :: !String } deriving (Show, Eq)

isBattery (Battery _ _ _ _) = True
isBattery _                 = False

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

powerSupplyPaths :: IO [FilePath]
powerSupplyPaths = dirContentsVisible sysDir

readPowerSupply :: FilePath -> IO PowerSupply
readPowerSupply path
    | "BAT" `matches` path = batteryFiles path >>= readBattery
    | "AC" `matches` path  = readAc path
    | "USB" `matches` path = return Usb
    | otherwise            = return $ Bad ""
    where matches s = (isPrefixOf s) . takeFileName

powerIcon :: [PowerSupply] -> String
powerIcon list | (not . null) $ filter (== Ac True) list = "\xf1e6" -- Active AC -> Plug
powerIcon list | (not . null) activeBats =
    if ratio < 1/8      then "\xf244" -- Empty
    else if ratio < 3/8 then "\xf243" -- Quarter
    else if ratio < 5/8 then "\xf242" -- Half
    else if ratio < 7/8 then "\xf241" -- 3/4
    else                     "\xf240" -- Full
    where
        active ps = status ps == "Discharging"
        activeBats = (filter active) . (filter isBattery) $ list
        ratio = (now (head activeBats)) / (full (head activeBats))
powerIcon list | [] /= filter (==Usb) list = "\xf287" -- USB logo
powerIcon _ = "\xf5d2" -- Unknown power supply -> Atom

details :: PowerSupply -> Maybe String
details (Battery full now power status)
    | status == "Full" = Just $ printf "%s Full" (alertHighlightLow ratio percentage)
    | otherwise = Just $ printf "%s %d:%02d" (alertHighlightLow ratio percentage) hoursLeft minsLeft
    where
        ratio = now / full
        percentage = alertHighlightLow ratio $ show (round $ ratio * 100) ++ "%"
        secsLeft = round $ now / power
        hoursLeft = secsLeft `div` 3600 :: Int
        minsLeft = (secsLeft `mod` 3600) `div` 60 :: Int
details _ = Nothing

readAc :: FilePath -> IO PowerSupply
readAc prefix = readFile (prefix </> "online") >>= return . (\content -> Ac (head content == '1'))

batteryFiles :: String -> IO Files
batteryFiles prefix = do
    { is_charge <- exists "charge_now"
    ; is_energy <- if is_charge then return False else exists "energy_now"
    ; is_power <- exists "power_now"
    ; plain <- exists (if is_charge then "charge_full" else "energy_full")
    ; let cf = if is_power then "power_now" else "current_now"
    ; let sf = if plain then "" else "_design"
    ; return $ case (is_charge, is_energy) of
        (True, _) -> files "charge" cf sf is_power
        (_, True) -> files "energy" cf sf is_power
        _ -> NoFiles
    } where
        exists = safeFileExist prefix
        files ch cf sf ip = Files
            { fFull = prefix </> ch ++ "_full" ++ sf
            , fNow = prefix </> ch ++ "_now"
            , fCurrent = prefix </> cf
            , fVoltage = prefix </> "voltage_now"
            , fStatus = prefix </> "status"
            , isCurrent = not ip
            }

readBattery :: Files -> IO PowerSupply
readBattery = readBattery' 1e6

readBattery' :: Float -> Files -> IO PowerSupply
readBattery' _ NoFiles = return $ Bad ""
readBattery' sc files =
    do a <- grab $ fFull files
       b <- grab $ fNow files
       d <- grab $ fCurrent files
       s <- grabs $ fStatus files
       let sc' = if isCurrent files then sc / 10 else sc
           a' = max a b -- sometimes the reported max charge is lower than
       return $ Battery (3600 * a' / sc') -- wattseconds
                        (3600 * b / sc') -- wattseconds
                        (d / sc') -- watts
                        s -- string: Discharging/Charging/Full
    where grab f = handle onError $ withFile f ReadMode (fmap read . hGetLine)
          onError = const (return (-1)) :: SomeException -> IO Float
          grabs f = handle onError' $ withFile f ReadMode hGetLine
          onError' = const (return "Unknown") :: SomeException -> IO String

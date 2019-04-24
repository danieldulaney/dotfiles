module FancyBattery
    ( FancyBattery (..)
    , batteryFiles
    , Files (..)
    , readBattery
    , acOnline
    , powerIcon
    ) where

import Xmobar hiding (main, Battery)

import Control.Exception (SomeException, handle)
import Control.Monad.ST
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.FilePath ((</>))
import System.Posix.Files
import Text.Printf

import Utils

data FancyBattery = FancyBattery
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyBattery where
    alias (FancyBattery _) = "fancybat"
    run   (FancyBattery _) = do
        { ac_status <- acOnline
        ; bat_status <- batteryFiles "BAT0" >>= readBattery
        ; return $ (powerIcon' ac_status bat_status) ++ (batDetails bat_status)
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

data Battery = Battery
    { full :: !Float
    , now :: !Float
    , power :: !Float
    , status :: !String
    } | NoBatt deriving (Show)

batDetails :: Battery -> String
batDetails NoBatt = ""
batDetails (Battery full now power _) = printf " %s %d:%02d" percentage hoursLeft minsLeft
    where
        ratio = now / full
        percentage = alertHighlightLow ratio $ show (round $ ratio * 100) ++ "%"
        secsLeft = round $ now / power
        hoursLeft = secsLeft `div` 3600 :: Int
        minsLeft = (secsLeft `mod` 3600) `div` 60 :: Int

powerIcon :: Bool -> Battery -> String
powerIcon True _ = "\xf1e6" -- AC plug icon
powerIcon False NoBatt = "\xf059" -- Question mark icon (no battery and no ac)
powerIcon False (Battery full now _ _)
    | ratio < 1/8 = "\xf244" -- Empty
    | ratio < 3/8 = "\xf243" -- Quarter
    | ratio < 5/8 = "\xf242" -- Half
    | ratio < 7/8 = "\xf241" -- Three-quarter
    | otherwise   = "\xf240" -- Full
    where ratio = now / full

powerIcon' ac = (concat . (map icon)) . (powerIcon ac)

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

acOnlineFile = sysDir </> "AC" </> "online"

acOnline :: IO Bool
acOnline = readFile acOnlineFile >>= return . (\content -> head content == '1')

safeFileExist :: String -> String -> IO Bool
safeFileExist d f = handle noErrors $ fileExist (d </> f)
    where noErrors = const (return False) :: SomeException -> IO Bool

batteryFiles :: String -> IO Files
batteryFiles bat = do
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
        prefix = sysDir </> bat
        exists = safeFileExist prefix
        files ch cf sf ip = Files
            { fFull = prefix </> ch ++ "_full" ++ sf
            , fNow = prefix </> ch ++ "_now"
            , fCurrent = prefix </> cf
            , fVoltage = prefix </> "voltage_now"
            , fStatus = prefix </> "status"
            , isCurrent = not ip
            }

readBattery :: Files -> IO Battery
readBattery = readBattery' 1e6

readBattery' :: Float -> Files -> IO Battery
readBattery' _ NoFiles = return $ NoBatt
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

module FancyNetwork where

import Xmobar

import System.Process
import Data.IP
import Data.List
import Data.Char

data FancyNetwork = FancyNetwork
    { interval :: Int
    } deriving (Read, Show)

instance Exec FancyNetwork where
    alias (FancyNetwork _) = "fancynet"
    run   (FancyNetwork _) = do
        { return "Get that network"
        }
    rate  (FancyNetwork i) = i

data ConnType = Ethernet | Wlan | Loopback | Other

data Interface = Interface
    { name :: String
    , connType :: ConnType
    , addresses :: [Address]
    }

data Address = Inet4
    { address :: IPv4
    , mask :: AddrRange IPv4
    }
    | OtherAddr
    deriving (Read, Show)

fetchAddressInfo :: IO String
fetchAddressInfo = readProcess "ip" ["addr", "show"] ""

-- A list of interfaces
-- Each interface has
-- * A header line
-- * Zero or more connection sections
groupInterfaceLines :: String -> [(String, [[String]])]
groupInterfaceLines =
      map (\(h, b) -> (h, groupBy (\_ next -> isSpace $ next!!4) b))
    . map (\lines -> (head lines, tail lines))
    . groupBy (\_ next -> isSpace $ head next)
    . lines

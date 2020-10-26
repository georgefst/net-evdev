module Main where

import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.Split
import Evdev
import Evdev.Codes
import Evdev.Stream
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import Streamly
import qualified Streamly.Prelude as S
import System.Process

data Args = Args
    { port :: Int
    , ip :: String
    }
    deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
    args <- getRecord "net-evdev"
    print args
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock $ SockAddrInet defaultPort 0
    let addr = SockAddrInet (fromIntegral $ port args) $ readIp $ ip args
    void $ S.foldlM' (uncurry . f sock addr) False $ S.map (second eventData) allEvs

type State = Bool

f :: Socket -> SockAddr -> State -> Device -> EventData -> IO State
f sock addr active dev = \case
    KeyEvent KeyRightalt t ->
        case t of
            Pressed -> do
                {-TODO the disabling isn't quite gonna work for bluetooth keyboard, with several devices in one
                    only disable the device which pressed altgr -}
                n <- deviceName dev --TODO cache (although tbh, while it may be IO, it should still be very cheap)
                callProcess "xinput" ["disable", C.unpack n] --TODO ByteString would be nice
                -- grabDevice dev -- this confuses X, unfortunately
                pure True
            Released -> do
                n <- deviceName dev
                callProcess "xinput" ["enable", C.unpack n]
                -- ungrabDevice dev
                pure False
            Repeated ->
                continue
    KeyEvent k t -> do
        --TODO there are some unsafe int conversions here
        when active $ sendEvent k t
        continue
    _ -> continue
  where
    continue = pure active -- don't change whether we are active
    sendEvent k t = void $ sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum t]) addr

allEvs :: IsStream t => t IO (Device, Event)
allEvs = readEventsMany $ allDevices <> newDevices

--TODO this isn't nice
readIp :: String -> HostAddress
readIp s = case map read $ splitOn "." s of
    [a, b, c, d] -> tupleToHostAddress (a, b, c, d)
    _ -> error "invalid ip"

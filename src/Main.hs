module Main where

import Control.Monad
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.List.Split
import Lens.Micro
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Process

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Streamly (IsStream)
import Streamly.Prelude qualified as S

import Evdev
import Evdev.Codes
import Evdev.Stream

{-TODO mirror Rust args
currently we always start idle and switch with right alt
device arg is irrelevant as here we always read from all
-}
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
        s =
            State
                { active = False -- currently grabbed and sending events
                , interrupted = False -- have there been any events from other keys since switch was last pressed?
                , hangingSwitch = False -- we don't necessarily want to send a switch down event, since we might actually
                -- be switching mode - so we carry over to the next round
                }
    void $ S.foldlM' (uncurry . f sock addr) s $ S.map (second eventData) allEvs

data State = State
    { active :: Bool
    , interrupted :: Bool
    , hangingSwitch :: Bool
    }
    deriving (Generic)

--TODO use State monad
f :: Socket -> SockAddr -> State -> Device -> EventData -> IO State
f sock addr s dev = \case
    KeyEvent key eventVal ->
        if key == KeyRightalt
            then case eventVal of
                Pressed -> pure if s ^. #active then s & #interrupted .~ False & #hangingSwitch .~ True else s
                Released ->
                    if s ^. #interrupted && s ^. #active
                        then sendKey key eventVal >> pure s
                        else do
                            let active' = not $ s ^. #active
                            devName <- deviceName dev
                            s' <-
                                if active'
                                    then do
                                        --TODO grab all
                                        callProcess "xinput" ["disable", C.unpack devName] --TODO ByteString would be nice
                                        C.putStrLn "Switched to active"
                                        pure s
                                    else do
                                        --TODO ungrab all
                                        callProcess "xinput" ["enable", C.unpack devName]
                                        C.putStrLn "Switched to idle"
                                        pure $ s & #hangingSwitch .~ False
                            pure $ s' & #active .~ active'
                Repeated -> continue
            else
                if s ^. #active
                    then do
                        when (s ^. #hangingSwitch) $ sendKey KeyRightalt Pressed
                        sendKey key eventVal
                        pure $ s & #interrupted .~ True & #hangingSwitch .~ False
                    else continue
    _ -> continue
  where
    sendKey k t = void $ sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum t]) addr
    continue = pure s -- don't change state

allEvs :: IsStream t => t IO (Device, Event)
allEvs = readEventsMany $ allDevices <> newDevices

--TODO this isn't nice
readIp :: String -> HostAddress
readIp s = case map read $ splitOn "." s of
    [a, b, c, d] -> tupleToHostAddress (a, b, c, d)
    _ -> error "invalid ip"

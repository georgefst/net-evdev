module Main (main) where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.State
import Data.Bifunctor
import Data.Bool
import Data.Generics.Labels ()
import Data.List.Split
import Lens.Micro.Platform hiding (both)
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Process

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Tuple.Extra (both)
import Streamly (SerialT)
import Streamly.Internal.Prelude (hoist)
import Streamly.Prelude qualified as S

import Evdev
import Evdev.Codes
import Evdev.Stream

import Orphans ()

data Args = Args
    { port :: Int
    , ip :: String
    , switchKey :: Key
    , idleCmd :: Maybe String
    , activeCmd :: Maybe String
    }
    deriving (Generic, Show)
instance ParseRecord Args where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
    Args{..} <- getRecord "net-evdev"
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock $ SockAddrInet defaultPort 0
    let addr = SockAddrInet (fromIntegral port) $ readIp ip
        s =
            AppState
                { active = False
                , interrupted = False
                , hangingSwitch = False
                }
        cmds = both (maybe mempty mkProcess) (idleCmd, activeCmd)
    void $ flip execStateT s $ S.mapM_ (uncurry $ f cmds switchKey sock addr) $ S.map (second eventData) allEvs

data AppState = AppState
    { active :: Bool -- currently grabbed and sending events
    , interrupted :: Bool -- have there been any events from other keys since switch was last pressed?
    , hangingSwitch :: Bool -- we don't necessarily want to send a switch down event, since we might actually
    -- be switching mode - so we carry over to the next round
    }
    deriving (Generic)

f :: (IO (), IO ()) -> Key -> Socket -> SockAddr -> Device -> EventData -> StateT AppState IO ()
f cmds switch sock addr dev = \case
    KeyEvent key eventVal ->
        if key == switch
            then case eventVal of
                Pressed -> whenM (use #active) do
                    #interrupted .= False
                    #hangingSwitch .= True
                Released -> ifM
                    (use #interrupted &&^ use #active)
                    do
                        sendKey key eventVal
                    do
                        #active %= not
                        use #active >>= liftIO . uncurry bool cmds
                        xinput dev =<< use #active
                        whenM (not <$> use #active) $ #hangingSwitch .= False
                Repeated -> pure ()
            else whenM (use #active) do
                whenM (use #hangingSwitch) $ sendKey switch Pressed
                sendKey key eventVal
                #interrupted .= True
                #hangingSwitch .= False
    _ -> pure ()
  where
    --TODO there are some unsafe int conversions here
    sendKey k t = liftIO . void $ sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum t]) addr

--TODO apply to all devices, and perhaps use evdev grab/ungrab
xinput :: MonadIO m => Device -> Bool -> m ()
xinput dev active' = liftIO do
    devName <- C.unpack <$> deviceName dev
    if active'
        then do
            callProcess "xinput" ["disable", devName] --TODO ByteString would be nice
            C.putStrLn "Switched to active"
        else do
            callProcess "xinput" ["enable", devName]
            C.putStrLn "Switched to idle"

allEvs :: forall m. MonadIO m => SerialT m (Device, Event)
allEvs = hoist liftIO . readEventsMany $ allDevices <> newDevices

--TODO Text/ByteString
mkProcess :: String -> IO ()
mkProcess s = case words s of
    [] -> error "empty process string"
    x : xs -> void . forkIO $ callProcess x xs

{-TODO this isn't nice
we should use newtype to get around the fact that the 'ParseField' instance betrays that 'type HostAddress = Word32'
-}
readIp :: String -> HostAddress
readIp s = case map read $ splitOn "." s of
    [a, b, c, d] -> tupleToHostAddress (a, b, c, d)
    _ -> error "invalid ip"

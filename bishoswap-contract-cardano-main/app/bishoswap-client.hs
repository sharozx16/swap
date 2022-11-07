{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                           (forM_, when)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (Result (..), ToJSON, decode, encode, fromJSON)
import qualified Data.ByteString.Lazy.Char8              as B8
import qualified Data.ByteString.Lazy                    as LB
import           Data.Monoid                             (Last (..))
import           Data.Proxy                              (Proxy (..))
import           Data.String                             (IsString (..))
import           Data.Text                               (Text, pack)
import           Data.UUID                               hiding (fromString)
import           Ledger.Value                            (AssetClass (..), CurrencySymbol, Value, flattenValue, TokenName)
import           Network.HTTP.Req
import qualified Plutus.Contracts.Uniswap                as BS
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           Text.Printf                             (printf)
import           Text.Read                               (readMaybe)
import           Wallet.Emulator.Types                   (Wallet (..))

import           Bashoswap                                 (cidFile, BashoswapContracts)

main :: IO ()
main = do
    w   <- Wallet . read . head <$> getArgs
    cid <- read                 <$> readFile (cidFile w)
    mcs <- decode               <$> LB.readFile "symbol.json"
    case mcs of
        Nothing -> putStrLn "invalid symbol.json" >> exitFailure
        Just cs -> do
            putStrLn $ "cid: " ++ show cid
            putStrLn $ "symbol: " ++ show (cs :: CurrencySymbol)
            go cid cs
  where
    go :: UUID -> CurrencySymbol -> IO a
    go cid cs = do
        cmd <- readCommandIO
        case cmd of
            Funds                    -> getFunds cid
            Pools                    -> getPools cid
            Create amtA tnA amtB tnB -> createPool cid $ toCreateParams cs amtA tnA amtB tnB
            Add amtA tnA amtB tnB    -> addLiquidity cid $ toAddParams cs amtA tnA amtB tnB
            Remove amt tnA tnB       -> removeLiquidity cid $ toRemoveParams cs amt tnA tnB
            Close tnA tnB            -> closePool cid $ toCloseParams cs tnA tnB
            Swap amtA tnA tnB        -> swap cid $ toSwapParams cs amtA tnA tnB
        go cid cs

data Command =
      Funds
    | Pools
    | Create Integer Char Integer Char
    | Add Integer Char Integer Char
    | Remove Integer Char Char
    | Close Char Char
    | Swap Integer Char Char
    deriving (Show, Read, Eq, Ord)

readCommandIO :: IO Command
readCommandIO = do
    putStrLn "Enter a command: Funds, Pools, Create amtA tnA amtB tnB, Add amtA tnA amtB tnB, Remove amt tnA tnB, Close tnA tnB, Swap amtA tnA tnB"
    s <- getLine
    maybe readCommandIO return $ readMaybe s

toCoin :: CurrencySymbol -> Char -> BS.Coin c
toCoin cs tn = BS.Coin $ AssetClass (cs, fromString [tn])

toCreateParams :: CurrencySymbol -> Integer -> Char -> Integer -> Char -> BS.CreateParams
toCreateParams cs amtA tnA amtB tnB = BS.CreateParams (toCoin cs tnA) (toCoin cs tnB) (BS.Amount amtA) (BS.Amount amtB)

toAddParams :: CurrencySymbol -> Integer -> Char -> Integer -> Char -> BS.AddParams
toAddParams cs amtA tnA amtB tnB = BS.AddParams (toCoin cs tnA) (toCoin cs tnB) (BS.Amount amtA) (BS.Amount amtB)

toRemoveParams :: CurrencySymbol -> Integer -> Char -> Char -> BS.RemoveParams
toRemoveParams cs amt tnA tnB = BS.RemoveParams (toCoin cs tnA) (toCoin cs tnB) (BS.Amount amt)

toCloseParams :: CurrencySymbol -> Char -> Char -> BS.CloseParams
toCloseParams cs tnA tnB = BS.CloseParams (toCoin cs tnA) (toCoin cs tnB)

toSwapParams :: CurrencySymbol -> Integer -> Char -> Char -> BS.SwapParams
toSwapParams cs amtA tnA tnB = BS.SwapParams (toCoin cs tnA) (toCoin cs tnB) (BS.Amount amtA) (BS.Amount 0)

showCoinHeader :: IO ()
showCoinHeader = printf "\n                                                 currency symbol                                                         token name          amount\n\n"

showCoin :: CurrencySymbol -> TokenName -> Integer -> IO ()
showCoin cs tn = printf "%64s %66s %15d\n" (show cs) (show tn)

getFunds :: UUID -> IO ()
getFunds cid = do
    callEndpoint cid "funds" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (BS.Funds v) -> showFunds v
            _                  -> go

    showFunds :: Value -> IO ()
    showFunds v = do
        showCoinHeader
        forM_ (flattenValue v) $ \(cs, tn, amt) -> showCoin cs tn amt
        printf "\n"

getPools :: UUID -> IO ()
getPools cid = do
    callEndpoint cid "pools" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (BS.Pools ps) -> showPools ps
            _                   -> go

    showPools :: [((BS.Coin BS.A, BS.Amount BS.A), (BS.Coin BS.B, BS.Amount BS.B))] -> IO ()
    showPools ps = do
        forM_ ps $ \((BS.Coin (AssetClass (csA, tnA)), amtA), (BS.Coin (AssetClass (csB, tnB)), amtB)) -> do
            showCoinHeader
            showCoin csA tnA (BS.unAmount amtA)
            showCoin csB tnB (BS.unAmount amtB)

createPool :: UUID -> BS.CreateParams -> IO ()
createPool cid cp = do
    callEndpoint cid "create" cp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right BS.Created -> putStrLn "created"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

addLiquidity :: UUID -> BS.AddParams -> IO ()
addLiquidity cid ap = do
    callEndpoint cid "add" ap
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right BS.Added -> putStrLn "added"
            Left err'      -> putStrLn $ "error: " ++ show err'
            _              -> go

removeLiquidity :: UUID -> BS.RemoveParams -> IO ()
removeLiquidity cid rp = do
    callEndpoint cid "remove" rp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right BS.Removed -> putStrLn "removed"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

closePool :: UUID -> BS.CloseParams -> IO ()
closePool cid cp = do
    callEndpoint cid "close" cp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right BS.Closed -> putStrLn "closed"
            Left err'       -> putStrLn $ "error: " ++ show err'
            _               -> go

swap :: UUID -> BS.SwapParams -> IO ()
swap cid sp = do
    callEndpoint cid "swap" sp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right BS.Swapped -> putStrLn "swapped"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

getStatus :: UUID -> IO (Either Text BS.UserContractState)
getStatus cid = runReq defaultHttpConfig $ do
    liftIO $ printf "\nget request to 127.0.1:9080/api/contract/instance/%s/status\n" (show cid)
    w <- req
        GET
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show cid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState BashoswapContracts)))
        (port 9080)
    case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (Last Nothing)  -> liftIO $ threadDelay 1_000_000 >> getStatus cid
        Success (Last (Just e)) -> return e
        _                       -> liftIO $ ioError $ userError "error decoding state"

callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()
callEndpoint cid name a = handle h $ runReq defaultHttpConfig $ do
    liftIO $ printf "\npost request to 127.0.1:9080/api/contract/instance/%s/endpoint/%s\n" (show cid) name
    liftIO $ printf "request body: %s\n\n" $ B8.unpack $ encode a
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack name)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ name
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show

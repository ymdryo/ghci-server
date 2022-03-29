{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Main where

import Src.GhciServer
import Network.GRPC.HighLevel.Generated
import System.Environment
import qualified Data.Text.Lazy as TL
import Control.Exception
import qualified Data.Text.Lazy.IO as TLIO
import System.IO (stderr)
import Control.Monad
import Data.Int
import Data.Function

main :: IO ()
main = do
   [cmd] <- getArgs

   let
      clientConfig =
         ClientConfig
            {  clientServerHost = "localhost"
            ,  clientServerPort = 50051
            ,  clientArgs = []
            ,  clientSSLConfig = Nothing
            ,  clientAuthority = Nothing
            }

      timeoutSeconds = fromIntegral (maxBound :: Int32)

   void $ withGRPCClient clientConfig $ \client -> do
      GhciService execute <- ghciServiceClient client
      res <- execute $ ClientReaderRequest
         (Command $ TL.pack cmd)
         timeoutSeconds
         mempty
         \_ _ recv ->
            fix \next ->
               recv >>= either throwIO pure >>= \case
                  Nothing -> pure ()
                  Just (ExecuteResponse output fd) -> do
                     case fd of
                        1 -> TLIO.putStrLn output
                        2 -> TLIO.hPutStrLn stderr output
                        fd -> TLIO.putStrLn $ "[warning: unknown fd(" <> TL.pack (show fd) <> ")]" <> output
                     next
      
      case res of
         ClientReaderResponse _ statusCode statusDetails ->
            when (statusCode /= StatusOk) do
               putStrLn $ show statusCode <> ", " <> show statusDetails
         
         ClientErrorResponse e -> TLIO.hPutStrLn stderr $ TL.pack $ show e
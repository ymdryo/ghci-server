{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Src.GhciServer
import Network.GRPC.HighLevel.Generated
import Control.Concurrent.Async
import Language.Haskell.Ghcid
import System.Process

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.IORef
import Data.Maybe
import Control.Monad.Extra
import Data.Functor
import qualified Data.Text.Encoding as BS
import Network.GRPC.LowLevel.Server
import GHC.IO.Exception

main :: IO ()
main = void do
   (ghci, loads) <- startGhciProcess
      (proc "stack" ["ghci", "--ghci-options", "-fdiagnostics-color=always"])
      (\_ _ -> pure ())
   
   commandInProgress <- newTVarIO False

   stopServerTMVar <- newEmptyTMVarIO

   let
      handlers :: GhciService ServerRequest ServerResponse
      handlers =
         GhciService
            {  ghciServiceExecute = ghciServiceExecuteHandler
            }

      ghciServiceExecuteHandler
         ::    ServerRequest 'ServerStreaming Command ExecuteResponse
            -> IO (ServerResponse 'ServerStreaming ExecuteResponse)
      ghciServiceExecuteHandler (ServerWriterRequest serverCall (Command cmd) send) = do
         commandInProgress' <- atomically $ swapTVar commandInProgress True

         if commandInProgress' then
            pure $ response StatusResourceExhausted $ StatusDetails "Command already in progress."
         else do
            grpcError <- newIORef Nothing

            let execute =
                  execStream ghci (TL.unpack cmd) \stream output ->
                     let
                        fd =
                           case stream of
                              Stdout -> 1
                              Stderr -> 2
                     in do
                        whenM (isNothing <$> readIORef grpcError) do
                           res <- send $ ExecuteResponse (TL.pack output) fd
                           case res of
                              Left e -> do
                                 interrupt ghci
                                 writeIORef grpcError $ Just e
                              Right () -> pure ()
            
            execute `catch` \UnexpectedExit{} -> atomically $ putTMVar stopServerTMVar ()

            atomically $ writeTVar commandInProgress False

            readIORef grpcError <&> \case
               Nothing -> response StatusOk $ StatusDetails mempty
               Just e -> response StatusCancelled $ StatusDetails $ BS.encodeUtf8 $ T.pack $ show e

         where
            response = ServerWriterResponse (metadata serverCall)

   grpcServer <- async $ ghciServiceServer handlers defaultServiceOptions

   waitEitherCancel grpcServer =<< async (atomically $ takeTMVar stopServerTMVar)

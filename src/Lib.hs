{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where
import Language.Haskell.Ghcid (Stream)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.IO (stderr)
import GHC.IO.FD (FD)

{-
printStream :: TL.Text -> FD -> IO ()
printStream output = \case
   1 -> TLIO.putStrLn output
   2 -> TLIO.hPutStrLn stderr output
   fd -> TLIO.putStrLn $ "[warning: unknown fd(" <> TL.pack (show fd) <> ")]" <> output
-}
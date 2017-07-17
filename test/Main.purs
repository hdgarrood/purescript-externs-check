module Test.Main where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Argonaut (jsonParser)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Node.FS (FS)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

import ExternsCheck (checkEntryPoint)

main :: forall e. Eff _ Unit
main = do
  jsonStr <- readTextFile UTF8 "./output/Test.Sample/externs.json"
  json <- either throw pure $ jsonParser jsonStr 

  logShow $ checkEntryPoint "ok_1" json
  logShow $ checkEntryPoint "notok_1" json

shouldBeNothing :: forall a. Show a => Maybe a -> Eff _ Unit
shouldBeNothing =
  maybe (pure unit) (\x -> throw ("Expected Nothing, got " <> show x))

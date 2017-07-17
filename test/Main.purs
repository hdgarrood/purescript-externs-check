module Test.Main where

import Prelude
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Either (either)
import Data.Array as Array
import Data.Argonaut (jsonParser)
import Data.Foldable (for_)
import Data.Validation.Semigroup (V, unV, isValid)
import Data.String (stripPrefix, Pattern(..))
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Node.FS (FS)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

import ExternsCheck (checkEntryPoint, UnsuitableReason, exportedValues)

main :: forall e. Eff _ Unit
main = do
  externsStr <- readTextFile UTF8 "./output/Test.Sample/externs.json"
  externs <- either throw pure $ jsonParser externsStr

  let cases = exportedValues externs

  log "Passing cases:"
  for_ (Array.filter isOk cases) \c -> do
    log ("  " <> c)
    shouldSucceed (checkEntryPoint c externs)

  log "Failing cases:"
  for_ (Array.filter isNotOk cases) \c -> do
    let v = checkEntryPoint c externs
    log ("  " <> c <> ":")
    log ("    " <> show v)
    shouldFail v

  log "Nonexistent entry point:"
  do
    let c = "nonexistent"
    let v = checkEntryPoint c externs
    log ("  " <> c <> ":")
    log ("    " <> show v)
    shouldFail v

  where
  isOk = isJust <<< stripPrefix (Pattern "ok")
  isNotOk = isJust <<< stripPrefix (Pattern "notok")

shouldSucceed :: V (Array UnsuitableReason) Unit -> Eff _ Unit
shouldSucceed =
  unV (\errs -> throw ("Expected no errors, got " <> show errs)) pure

shouldFail :: V (Array UnsuitableReason) Unit -> Eff _ Unit
shouldFail v =
  if isValid v then throw "Expected errors, got none" else pure unit

shouldFailWith :: Array UnsuitableReason -> V (Array UnsuitableReason) Unit -> Eff _ Unit
shouldFailWith exp =
  unV (\act -> when (exp `differentFrom` act)
                   (throw ("Expected " <> show exp <> ", got " <> show act)))
      (const (throw "Expected errors, got none"))

  where
  differentFrom xs ys = Array.sort xs /= Array.sort ys

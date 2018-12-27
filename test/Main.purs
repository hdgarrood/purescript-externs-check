module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Data.Argonaut (jsonParser)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.String (stripPrefix, Pattern(..))
import ExternsCheck (UnsuitableReason, checkEntryPoint, defaultOptions, exportedValues, FQName(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

goodEff = FQName "Test.Good.Eff"

testOptionsFor c =
  { mainName: c
  , typeConstructors: pure goodEff <> defaultOptions.typeConstructors
  }

main :: Effect Unit
main = do
  externsStr <- readTextFile UTF8 "./output/Test.Sample/externs.json"
  externs <- either throw pure $ jsonParser externsStr

  let cases = exportedValues externs

  log "Passing cases:"
  for_ (Array.filter isOk cases) \c -> do
    log ("  " <> c)
    shouldSucceed (checkEntryPoint (testOptionsFor c) externs)

  log "Failing cases:"
  for_ (Array.filter isNotOk cases) \c -> do
    let v = checkEntryPoint (testOptionsFor c) externs
    log ("  " <> c <> ":")
    log ("    " <> show v)
    shouldFail v

  log "Nonexistent entry point:"
  do
    let c = "nonexistent"
    let v = checkEntryPoint (testOptionsFor c) externs
    log ("  " <> c <> ":")
    log ("    " <> show v)
    shouldFail v

  where
  isOk = isJust <<< stripPrefix (Pattern "ok")
  isNotOk = isJust <<< stripPrefix (Pattern "notok")

shouldSucceed :: Either (NonEmptyArray UnsuitableReason) Unit -> Effect Unit
shouldSucceed =
  either (\errs -> throw ("Expected no errors, got " <> show errs)) pure

shouldFail :: Either (NonEmptyArray UnsuitableReason) Unit -> Effect Unit
shouldFail =
  either (\_ -> pure unit) (\_ -> throw "Expected errors, got none")

shouldFailWith :: NonEmptyArray UnsuitableReason -> Either (NonEmptyArray UnsuitableReason) Unit -> Effect Unit
shouldFailWith exp =
  either (\act -> when (exp `differentFrom` act)
                       (throw ("Expected " <> show exp <> ", got " <> show act)))
         (\_ -> (throw "Expected errors, got none"))

  where
  differentFrom xs ys = NonEmptyArray.sort xs /= NonEmptyArray.sort ys

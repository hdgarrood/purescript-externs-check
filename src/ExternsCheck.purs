module ExternsCheck
  ( checkEntryPoint
  , Options
  , defaultOptions
  , UnsuitableReason(..)
  , FQName(..)
  , typeEff
  , exportedValues
  ) where

import Prelude

import Control.MonadPlus (guard, (<|>))
import Data.Newtype (class Newtype, unwrap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Data.String as String
import Data.Array ((!!))
import Data.Foldable (find)
import Data.Traversable (traverse)
import Data.StrMap as StrMap
import Data.Argonaut (Json, foldJsonObject, toArray, toString)
import Data.Validation.Semigroup (V, unV, invalid)

-- | Options for checking an entry point.
-- |
-- | The `typeConstructor` option allows you to pick a single type constructor
-- | which the entry point should be using; usually this will be
-- | `Control.Monad.Eff.Eff`, but you may want to use an alternative type. Note
-- | however, that whichever type you use, you should ensure that its runtime
-- | representation is the same as `Eff`, in that it should be a function which
-- | executes your program when it is called with no arguments, as most
-- | PureScript tooling will assume that this is the case.
-- |
-- | The `mainName` option specifies the name of the entry point value; usually
-- | "main".
type Options
  = { typeConstructor :: FQName
    , mainName :: String
    }

-- | Default `Options` for an entry point check. Using these `Options` will
-- | check that your entry point module exports a `main` value whose type is
-- | `Eff`.
defaultOptions :: Options
defaultOptions =
  { typeConstructor: typeEff
  , mainName: "main"
  }

-- | Given a module's externs JSON, check that it exports a value with the
-- | name specified in the `Options`, and also that the value is suitable for
-- | use as a program's entry point (based on comparing its type in the externs
-- | file to the type specified in the `Options`).
checkEntryPoint :: Options -> Json -> Either (Array UnsuitableReason) Unit
checkEntryPoint opts json =
  unV Left Right (checkEntryPointV opts json)

checkEntryPointV :: Options -> Json -> V (Array UnsuitableReason) Unit
checkEntryPointV { typeConstructor, mainName } json =
  case findTypeOf json mainName of
    Just ty -> checkSuitableMain typeConstructor ty
    Nothing -> invalid [NoExport]

-- | Some JSON representing a type in a PureScript externs file.
newtype Type = Type Json

derive instance newtypeType :: Newtype Type _
derive instance genericType :: Generic Type _

-- | A fully-qualified name (eg. for a value or a type class).
newtype FQName = FQName String

derive instance newtypeFQName :: Newtype FQName _
derive instance genericFQName :: Generic FQName _
derive newtype instance eqFQName :: Eq FQName
derive newtype instance showFQName :: Show FQName
derive newtype instance ordFQName :: Ord FQName

-- | A `FQName` representing `Eff` from `Control.Monad.Eff`. This is defined as
-- |
-- |     FQName "Control.Monad.Eff.Eff"
typeEff :: FQName
typeEff = FQName "Control.Monad.Eff.Eff"

-- | Given the JSON in an externs file, find the type of the identifier
-- | with the given name (if any).
findTypeOf :: Json -> String -> Maybe Type
findTypeOf externs ident = go externs
  where
  go =
    prop "efDeclarations"
    >=> toArray
    >=> find (nameIs ident)
    >=> getType

-- | Return a list of exported values from a module, given its externs file
exportedValues :: Json -> Array String
exportedValues externs =
  let
    exports =
      fromMaybe [] (prop "efExports" externs >>= toArray)

    maybeToArray =
      maybe [] (\x -> [x])
  in
    exports >>= (maybeToArray <<< getIdent)

  where
  getIdent :: Json -> Maybe String
  getIdent =
    prop "ValueRef"
    >=> toArray
    >=> (_ !! 1)
    >=> prop "Ident"
    >=> toString

prop :: String -> Json -> Maybe Json
prop i = foldJsonObject Nothing (StrMap.lookup i)

nameIs :: String -> Json -> Boolean
nameIs ident decl =
  go decl == Just ident
  where
  go =
    prop "EDValue"
    >=> prop "edValueName"
    >=> prop "Ident"
    >=> toString

getType :: Json -> Maybe Type
getType =
  prop "EDValue"
  >=> prop "edValueType"
  >=> (pure <<< Type)

checkSuitableMain :: FQName -> Type -> V (Array UnsuitableReason) Unit
checkSuitableMain name ty =
  checkConstraints ty *> checkMatches name ty

checkConstraints :: Type -> V (Array UnsuitableReason) Unit
checkConstraints ty =
  case getConstraints ty of
    [] -> pure unit
    cs -> invalid [Constraints cs]

checkMatches :: FQName -> Type -> V (Array UnsuitableReason) Unit
checkMatches name ty =
  case getHeadTypeCon ty of
    Just h ->
      if h == name
        then pure unit
        else invalid [TypeMismatch (Just h)]
    Nothing ->
      invalid [TypeMismatch Nothing]

-- | If the JSON has a "tag" property matching the given string, attempt to
-- | extract and decode the "contents" property as an array of JSON objects.
extractTaggedContents :: String -> Json -> Maybe (Array Json)
extractTaggedContents tag j = do
  tag' <- prop "tag" j >>= toString
  guard (tag == tag')
  prop "contents" j >>= toArray

getConstraints :: Type -> Array FQName
getConstraints =
  stripForalls >>> go >>> _.acc
  where
  go (Type ty) =
    repeatedly extractConstraint { acc: [], ty }

  extractConstraint { acc, ty } = do
    contents <- extractTaggedContents "ConstrainedType" ty
    constraint <- contents !! 0
    cls <- prop "constraintClass" constraint >>= toArray >>= toFQName
    inner <- contents !! 1
    pure { acc: acc <> [cls], ty: inner }

getHeadTypeCon :: Type -> Maybe FQName
getHeadTypeCon = stripForalls >>> unwrap >>> go
  where
  go ty =
    extractTyCon ty <|> recurseTypeApp ty <|> recurseConstraint ty

  extractTyCon ty =
    pure ty
    >>= extractTaggedContents "TypeConstructor"
    >>= toFQName

  recurseTypeApp ty =
    pure ty
    >>= extractTaggedContents "TypeApp"
    >>= (_ !! 0)
    >>= go

  recurseConstraint ty =
    pure ty
    >>= extractTaggedContents "ConstrainedType"
    >>= (_ !! 1)
    >>= go

toFQName :: Array Json -> Maybe FQName
toFQName j = do
  mn <- (j !! 0) >>= toArray
                 >>= traverse toString
                 >>= (pure <<< String.joinWith ".")
  name <- (j !! 1) >>= toString
  pure (FQName (mn <> "." <> name))

stripForalls :: Type -> Type
stripForalls (Type ty) = Type (go ty)
  where
  go = repeatedly stripForall

  stripForall = extractTaggedContents "ForAll" >=> (_ !! 1)

-- | Keep applying a function until it returns Nothing
repeatedly :: forall a. (a -> Maybe a) -> a -> a
repeatedly f = go
  where
  go x =
    case f x of
      Just y -> go y
      Nothing -> x

data UnsuitableReason
  = Constraints (Array FQName)
  | TypeMismatch (Maybe FQName)
  | NoExport

derive instance eqUnsuitableReason :: Eq UnsuitableReason
derive instance ordUnsuitableReason :: Ord UnsuitableReason
derive instance genericUnsuitableReason :: Generic UnsuitableReason _

instance showUnsuitableReason :: Show UnsuitableReason where
  show = genericShow

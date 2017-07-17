module ExternsCheck where

import Prelude

import Control.MonadPlus (guard, (<|>))
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.String as String
import Data.Array ((!!))
import Data.Foldable (find)
import Data.Traversable (traverse)
import Data.StrMap as StrMap
import Data.Argonaut (Json, foldJsonObject, toArray, toString)

-- | Check that the externs JSON for the given module exports an entry point
-- | with the given name, and that its type is suitable for use as a program's
-- | entry point.
-- |
-- | Returns Nothing if it looks OK, or otherwise Just with a reason.
checkEntryPoint :: String -> Json -> Maybe UnsuitableReason 
checkEntryPoint ident json =
  case findTypeOf json ident of
    Just ty -> checkSuitableMain ty
    Nothing -> Just NoExport

-- | Some JSON representing a type in a PureScript externs file.
newtype Type = Type Json

derive instance newtypeType :: Newtype Type _

-- | A fully-qualified name (eg. for a value or a type class).
newtype FQName = FQName String

derive instance newtypeFQName :: Newtype FQName _
derive newtype instance eqFQName :: Eq FQName
derive newtype instance showFQName :: Show FQName

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
    >=> find (\j -> nameIs ident j)
    >=> getType

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

checkSuitableMain :: Type -> Maybe UnsuitableReason
checkSuitableMain ty =
  eitherToMaybe do
    checkConstraints ty
    checkIsEff ty

  where
  eitherToMaybe :: forall l. Either l Unit -> Maybe l
  eitherToMaybe = either Just (const Nothing)

checkConstraints :: Type -> Either UnsuitableReason Unit
checkConstraints ty =
  case getConstraints ty of
    [] -> Right unit
    cs -> Left (Constraints cs)

checkIsEff :: Type -> Either UnsuitableReason Unit
checkIsEff ty =
  case getHeadTypeCon ty of
    Just h ->
      if h == typeEff
        then Right unit
        else Left (NotEff (Just h))
    Nothing ->
      Left (NotEff Nothing)

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
  | NotEff (Maybe FQName)
  | NoExport

instance showUnsuitableReason :: Show UnsuitableReason where
  show (Constraints cs) = "Unexpected constraints: " <> show cs
  show (NotEff (Just n)) = "Expected Eff, found " <> show n
  show (NotEff Nothing) = "Expected Eff"
  show NoExport = "Entry point not found"

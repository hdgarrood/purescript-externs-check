module Test.Good where

foreign import kind Effect

data Eff (eff :: # Effect) (a :: Type)

infix 5 type Eff as ^^^

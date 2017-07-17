module Test.Sample where

import Prelude
import Control.Monad.Eff (Eff)
import Test.MyEff as MyEff
import Unsafe.Coerce (unsafeCoerce)

class C (f :: Type -> Type)

infix 5 type Eff as ^^^

undefined :: forall a. a
undefined = unsafeCoerce unit

ok_1 :: Eff () Unit
ok_1 = undefined

ok_2 :: forall e. Eff e Unit
ok_2 = undefined

ok_3 :: forall e a. Eff e a
ok_3 = undefined

ok_4 :: forall a. Eff () a
ok_4 = undefined

ok_5 :: forall e a. e ^^^ a
ok_5 = undefined

notok_1 :: forall e. Partial => Eff e Unit
notok_1 = undefined

notok_2 :: forall f. f Unit
notok_2 = undefined

notok_3 :: forall f. C f => f Unit
notok_3 = undefined

notok_4 :: forall e a. Semiring a => Eff e a
notok_4 = undefined

notok_5 :: MyEff.Eff
notok_5 = undefined

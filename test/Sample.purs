module Test.Sample where

import Prelude

import Effect (Effect)
import Test.Bad as Bad
import Test.Good (Eff, type (^^^))
import Unsafe.Coerce (unsafeCoerce)

class C (f :: Type -> Type)

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

ok_6 :: Effect Unit
ok_6 = undefined

ok_7 :: Effect Int
ok_7 = pure 0

notok_1 :: forall e. Partial => Eff e Unit
notok_1 = undefined

notok_2 :: forall f. f Unit
notok_2 = undefined

notok_3 :: forall f. C f => f Unit
notok_3 = undefined

notok_4 :: forall e a. Semiring a => Eff e a
notok_4 = undefined

notok_5 :: Bad.Eff
notok_5 = undefined

notok_6 :: Semiring Int => Eff () Int
notok_6 = undefined

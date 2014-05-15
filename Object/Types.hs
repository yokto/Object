{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module Object.Types where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

type family Output object action

class Action object action where
	(.) :: object -> action -> Output object action

data a := b = a := b deriving (Show,Read,Eq)

type family MethodOutput object method

(%) = flip ($)
a ... b = \x -> a (b x)

-- |
-- Explanation of all these operators.
-- They are basically an assortment of 'Functor' and
-- 'Applicative' functions. I.e.,
-- '[1,2,3] *> (+1)' === map (+1) [1,2,3]'
-- The asterisk '*' indicates where the 'Functor', 'Applicative' is.
-- And the greather than (smaller than) indicates in which direction
-- the argument is going. So you could also have
-- '[(+1),(+2),(+3)] *< 1 === map ($ 1) [(+1),(+2),(+3)]'.
-- If there are asterisks on both sides the 'Functor' has to be 'Applicative'
a *> b = fmap (%b) a
a *< b = fmap ($ b) a -- space is in case of TH
a *. b = fmap (.b) a
a >* b = fmap (a%) b
a <* b = fmap (a$) b
a .* b = fmap (a.) b
a *>* b = (%) <$> (a) <*> b
a *<* b = ($) <$> a <*> b
a *.* b = (.) <$> a <*> b


infixl 8 .
infixl 9 :=
infixl 0 %

infixl 4 *.
infixl 4 .*
infixl 4 *.*

infixl 4 *>
infixl 4 >*
infixl 4 *>*

infixr 4 *<
infixr 4 <*
infixr 4 *<*
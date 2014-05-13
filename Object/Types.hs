{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module Object.Types where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

type family Restrict object action
type family Output object action

class Action object action where
	(.) :: object -> action -> Output object action

data a := b = a := b deriving (Show,Read,Eq)

(%) = flip ($)
a ... b = \x -> a (b x)

a >% b = fmap (%b) a
a >$ b = fmap ($b) a
a >. b = fmap (.b) a
a %< b = fmap (a%) b
a $< b = fmap (a$) b
a .< b = fmap (a.) b
a >%< b = (%) <$> (a) <*> b
a >$< b = ($) <$> a <*> b
a >.< b = (.) <$> a <*> b

a >%% b = join $ a >% b
a %%< b = join $ a %< b
a >%%< b = join $ a >%< b
a >$$ b = join $ a >$ b
a $$< b = join $ a $< b
a >$$< b = join $ a >$< b
a >.. b = join $ a >. b
a ..< b = join $ a .< b
a >..< b = join $ a >.< b

infixl 9 .
infixl 1 >.
infixl 1 .<
infixl 1 >.<
infixl 1 >..
infixl 1 ..<
infixl 1 >..<

infixl 1 %
infixl 1 >%
infixl 1 %<
infixl 1 >%<
infixl 1 >%%
infixl 1 %%<
infixl 1 >%%<

infixr 0 >$
infixr 0 $<
infixr 0 >$<
infixr 0 >$$
infixr 0 $$<
infixr 0 >$$<
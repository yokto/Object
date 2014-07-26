{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module Object.Types where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

class Action object action output where
	(.) :: object -> action -> output

data a := b = a := b deriving (Show,Read,Eq)

infixl 8 .
infixl 9 :=
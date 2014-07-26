{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-- | This module Just reexports Object.Types and Object.Templates
module Object(
    module Object.Types,
    module Object.Templates
    ) where

import Object.Types
import Object.Templates

-- |
-- function composition
instance (a~a', b' ~ b, c'~c) => Action (b -> c) (a -> b') (a' -> c') where
	f . g = f Prelude.. g
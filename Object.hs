{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-- | This module Just reexports Object.Types and Object.Templates
module Object(
    module Object.Types,
    module Object.Templates
    ) where

import Object.Types
import Object.Templates

type instance Output (b -> c) (a -> b) = (a -> c)
instance Action (b -> c) (a -> b) where
	f . g = f Prelude.. g
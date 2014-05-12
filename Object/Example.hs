{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeOperators, DataKinds, DeriveGeneric, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, DataKinds, UndecidableInstances, OverlappingInstances #-}
module Object.Example where

import Object.Templates
import Object.Types
import Object.Letters

import Prelude hiding ((.))
import Data.Maybe
import Data.Map
import Language.Haskell.TH

data Foo b = Foo {
	_bar ::Int,
	_baz :: Char,
	_blub :: b
	} deriving (Show)
    
-- $(reify ''Foo >>= (\info -> (reportWarning $ show info)) >> return [])


{-
instance Action Foo [AssociateObject Foo (Foo -> Foo)] where
	type Output Foo [AssociateObject Foo (Foo -> Foo)] = Foo
	obj . (AssociateObject set:rest) = set obj . rest
	obj . [] = obj
-}
setGetFunctional ''Foo

{-
type instance Restrict Foo [AssociateObject foo method] = [AssociateObject Foo method]
-}
--restrictAssociateObjectList ''Foo

{-
type Bar = (T_b,T_a,T_r)
bar = (T_b,T_a,T_r) :: Bar
...
-}
makeMethodVariable "bar"
makeMethodVariable "baz"
makeMethodVariable "blub"

{-
type instance Restrict Foo Bar = Bar
instance Action Foo Bar where
	type Output Foo Bar = Int
	obj . _ = _bar obj
-}

-- instance SetClass Foo Bar where
-- 	type SetValue Foo Bar = Int
-- 	type SetOutput Foo Bar = (Foo -> Foo)
-- 	_ .= val = AssociateObject $ \foo -> foo { _bar = val }

-- instance RestrictClass (Map k v) ()
-- instance Action (Map k v) () where
-- 	type Output (Map k v) () = [(k,v)]
-- 	map . _ = toList map
	

-- type family IsOrd o :: Bool
-- type instance IsOrd (T_Container a) = False
-- type instance (Ord o) IsOrd o = True

-- instance (k'~Just RestrictClass (Map k v) [k' := v']

-- instance RestrictClass (Map k v) [k' := v'] where
-- 	type Restrict (Map k v) [k' := v'] = Just [k := v]

main = do
	let
		map1 = empty . ['a' := "first_a", 'c' := "first_a"]
		res1 = map1 . ['a','b','c']
		map2 = map1 . ('a' := "secont_a")
		res2 = map2 . 'a'
		map3 = (empty:: Map Int String) . [(1::Int) := "hallo" ]
		res3 = map3 . [1 :: Int]
	print map1
	print res1
	print map2
	print res2
	print map3
	print res3


type instance Restrict (Map k v) action = RestrictMap (Map k v) action

type family RestrictMap map action where
	RestrictMap (Map k v) (Method a) = (Map k v, Method a)
	RestrictMap (Map k v) [k' := v'] = (Map k v,[k := v])
	RestrictMap (Map k v) (k' := v') = (Map k v,(k := v))
	RestrictMap (Map k v) (Maybe k') = (Map k v,Maybe k)
	RestrictMap (Map k v) [k'] = (Map k v,[k])
	RestrictMap (Map k v) k' = (Map k v,k)

type instance Output (Map k v) action = OutputMap (Map k v) action

type family OutputMap map action where
	OutputMap (Map k v) [k' := v'] = (Map k v)
	OutputMap (Map k v) (k' := v') = (Map k v)
	OutputMap (Map k v) (Maybe k') = Maybe v
	OutputMap (Map k v) [k'] = [v]
	OutputMap (Map k v) k' = v

	
instance (Restrict (Map k v) [k := v] ~ (Map k v,[k := v]),Ord k) => Action (Map k v) [k := v] where
	map . (k := v:rest) = insert k v map . rest
	map . [] = map

instance (Restrict (Map k v) (k := v) ~ (Map k v,(k := v)),Ord k) => Action (Map k v) (k := v) where
	map . assign = map . [assign]

instance (Restrict (Map k v) (Maybe k) ~ (Map k v,(Maybe k)),Ord k) => Action (Map k v) (Maybe k) where
	map . (Just key) = Data.Map.lookup key map
	map . Nothing = Nothing

instance (Restrict (Map k v) [k] ~ (Map k v,[k]),	OutputMap (Map k v) [k] ~ [v],Ord k) => Action (Map k v) [k] where
	map . keys = catMaybes $ flip Data.Map.lookup map $< keys

instance (Restrict (Map k v) k ~ (Map k v,k),OutputMap (Map k v) k ~ v,Ord k) => Action (Map k v) k where
	map . key = fromMaybe (error "no element of this key in map") $ map . Just key

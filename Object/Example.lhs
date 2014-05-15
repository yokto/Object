>{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeOperators, DataKinds, DeriveGeneric, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, DataKinds, UndecidableInstances, OverlappingInstances #-}
>module Object.Example where

>import Object.Templates
>import Object.Types
>import Object.Letters

>import Prelude hiding ((.))
>import Data.Maybe
>import qualified Data.Map as Map
>import Data.Map (Map)

In this example file we will see how to make objects
out of two data type 'Simple' and 'Foo x'

>data Simple = Simple { _simple :: Int } deriving (Show)

>data Foo x = Foo {
>	_bar ::Int,
>	_baz :: Char,
>	_blub :: x
>	} deriving (Show)

To make 'Foo x' we use templateHaskell.

>makeObject ''Foo

Poof, and we're done.
For 'Simple' we will do everything manually so you can see what happenes behind 'makeObject'.
We will want to make instances of the following two things that are defined in Object.Types.

type family Output object action

class Action object action where
	(.) :: object -> action -> Output object action

Now 'makeObject' makes two types of 'Actions' actions called methods
(where action has type 'Method m' for some m) and actions called methodsets
(where action has type 'Method m := input).

Unless you want to use a closed type class later the following definition would not be neccessary.
So if you are new you can imagine 'Output' and 'MethodOutput' are the same thing. For the pros
the following two lines are not defined if you use 'makeObjectFlexible'.

>type instance Output Simple (Method m) = MethodOutput Simple (Method m)
>type instance Output Simple (Method m := input) = MethodOutput Simple (Method m := input)

First we need a method name for '_simple'

>type Simple_ = Method (T_s,T_i,T_m,T_p,T_l,T_e)
>simple = Method (T_s,T_i,T_m,T_p,T_l,T_e) :: Simple_

it is done this way so different modules can define the same name
you can use the macro makeName to produce the same thing.

So now we can implement a method we only need to make an instance of 'Action' and 'Output' or
since 'Output' was defered of 'MethodOutput'

the output will be an Int since _simple has type Int.

>type instance MethodOutput Simple Simple_ = Int

and the implementation will simply be a call to '_simple'

>instance Action Simple Simple_ where
>	object . _ = _simple object

we can ignore the argument since we know it from the type

Now we can also make the methodset

>type instance MethodOutput Simple (Simple_  := anything) = Simple
>instance (anything ~ Int) => Action Simple (Simple_ := anything) where
>	object . (_ := int) = object { _simple = int }

Notice how we match anything and only in the context make sure that it is in deed Int.
This is because other wise GHC would give some ambiguity error since someone else could
possibly add an instance

instance Action Simple (Simple_ := Float)

So let's see what we can do so far.

>fooAndSimple = do
>	print$ Foo 1 'a' 1 . blub := 2 . blub -- 2
>	print$ Simple 1 . simple -- 1

Pretty cool. Ok now something a bit more complicated for 'Data.Map'.

>type instance Output (Map k v) action = OutputMap (Map k v) action

>type family OutputMap map action where
>	OutputMap (Map k v) (Method m) = MethodOutput (Map k v) (Method m)
>	OutputMap (Map k v) [k' := v'] = (Map k v)
>	OutputMap (Map k v) (k' := v') = (Map k v)
>	OutputMap (Map k v) (Maybe k') = Maybe v
>	OutputMap (Map k v) [k'] = [v]
>	OutputMap (Map k v) k' = v

>makeName "size"
>type instance MethodOutput (Map k v) Size_ = Int
>instance Action (Map k v) Size_ where
>	obj . _ = Map.size obj

>instance (k~k', v~v', OutputMap (Map k v) [k' := v'] ~ (Map k v), Ord k) => Action (Map k v) [k' := v'] where
>	map . (k := v:rest) = Map.insert k v map . rest
>	map . [] = map

>instance (k~k', v~v', OutputMap (Map k v) (k' := v') ~ (Map k v), Ord k) => Action (Map k v) (k' := v') where
>	map . assign = map . [assign]

>instance (k~k', OutputMap (Map k v) (Maybe k') ~ (Maybe v), Ord k) => Action (Map k v) (Maybe k') where
>	map . (Just key) = Map.lookup key map
>	map . Nothing = Nothing

>instance (k~k', OutputMap (Map k v) [k'] ~ [v], Ord k) => Action (Map k v) [k'] where
>	map . keys = catMaybes $ flip Map.lookup map <* keys

>instance (k~k', OutputMap (Map k v) k' ~ v, Ord k) => Action (Map k v) k' where
>	map . key = fromMaybe (error "no element of this key in map") $ map . Just key

>mapExample = do
>	let aMap = Map.empty . ['a' := "first_a", 'b' := "first_b"]
>	print$ aMap . ('a' := "secont_a") . 'a' -- "second_a
>	print$ aMap . ['a','b','c'] -- ["first_a","first_b"]
>	print$ aMap . size -- 2
>	print$ Map.empty . [(1::Int) := "hallo" ] . (1::Int) -- "hallo"
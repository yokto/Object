>{-# LANGUAGE
>	TemplateHaskell,
>	FlexibleInstances,
>	TypeOperators,
>	MultiParamTypeClasses,
>	FlexibleContexts,
>	TypeFamilies,
>	UndecidableInstances,
>	IncoherentInstances,
>	OverlappingInstances #-}
>module Object.Example where

>import Object
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
We will want to make instances of Action which is defined in Object.Types.

class Action object action output where
	(.) :: object -> action -> output

Now 'makeObject' makes get and set actions.

First we need a name for '_simple'

>type Simple_ = Method (T_s,T_i,T_m,T_p,T_l,T_e)
>simple = Method (T_s,T_i,T_m,T_p,T_l,T_e) :: Simple_

it is done this way so different modules can define the same name
you can use the macro 'makeName' to produce the same thing.

So now we can implement a method we only need to make an instance of 'Action',
and the implementation will simply be a call to '_simple'.

>instance (int ~ Int) => Action Simple Simple_ int where
>	object . _ = _simple object

We can ignore the argument since we know it from the type.
Also, notice that we didn't match for 'Int' but the type variable 'int'.
This is done so we can guarantee that this instance matches even
if the compiler doesn't know the output is an 'Int'.

We can also make the get Action.

>instance (int ~ Int, simple ~ Simple) => Action Simple (Simple_ := int) simple where
>	object . (_ := int) = object { _simple = int }

If you don't want to specify types it is important to make everything that could be polimorphic,
polimorphic in your instance. Otherwise GHC will give some ambiguity error since someone else could
possibly add an instance.

>fooAndSimple = do
>	print$ Foo 1 'a' 1 . blub := 2 . blub -- 2
>	print$ Simple 1 . simple := 3 . simple -- 3

Pretty cool. Ok now something a bit more complicated for 'Data.Map'.

>makeName "size"
>instance (int ~ Int) => Action (Map k v) Size_ int where
>	obj . _ = Map.size obj

>instance (k'~k, v'~v, outMap ~ (Map k v), Ord k) => Action (Map k v) [k' := v']  outMap where
>	map . (k := v:rest) = Map.insert k v map . rest
>	map . [] = map

>instance (k'~k, v'~v, outMap ~ (Map k v), Ord k) => Action (Map k v) (k' := v') outMap where
>	map . assign = map . [assign]

>instance (k'~k, maybe_v ~ (Maybe v), Ord k) => Action (Map k v) (Maybe k') maybe_v where
>	map . (Just key) = Map.lookup key map
>	map . Nothing = Nothing

>instance (k'~k, vs ~ [v], Ord k) => Action (Map k v) [k'] vs where
>	map . keys = catMaybes $ flip Map.lookup map `fmap` keys

>instance (k'~k, v'~v, Ord k) => Action (Map k v) k' v' where
>	map . key = fromMaybe (error "no element of this key in map") $ map . Just key

>mapExample = do
>	let aMap = Map.empty . ['a' := "first_a", 'b' := "first_b"]
>	print$ aMap . ('a' := "secont_a") . 'a' -- "second_a
>	print$ aMap . ['a','b','c'] -- ["first_a","first_b"]
>	print$ aMap . size -- 2
>	print$ Map.empty . [1 := "hallo" ] . 1 -- "hallo"
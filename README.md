# object

Object oriented programming for haskell

## Idea

The idea is to be able to use multiparameter typeclasses to create a dot operator which can access certain properties of the object/datatype. We give a simple example of the kind of thing that we want to be able to write the following.

	> :m -Prelude
	> import Prelude hiding ((.))
	> import Object
	> import Object.Example
	> import Data.Map hiding (size)
	> let m = empty . [ 'f' := Just 1, 'o' := Just 2, 'o' := Nothing ]
	> m
	fromList [('f',Just 1),('o',Nothing)]
	> m . 'f'
	Just 1
	> m . size
	2

You can run this example in ghci it should work you need ghc-7.8.

Ok so let us define the dot

	class Action object action where
		type Output object action
		(.) :: object -> action -> Output object action

I decided to make the Output an associated type instead of a parameter. This means the Output type can be calculated from the type of the object and the type of the action. The rest of this page will be concerned with working out some of the details (mostly ambiguous types). I also want to stress here that I want *as much as possible to work without type signatures*.

## Open world 1 and more type families

Ok so let us first implement the lookup since this is the easiest. This lets us get a value by specifying a key.

	instance (Ord key) => Action (Map key value) key where
		type Output (Map key value) key = value
		map . key = fromMaybe (error "no element of this key in map") $ lookup key map

Let's test if it works

	> fromList [('f',1)] . 'f'
	1
	> empty . 'f'
    No instance for (Action (Map k0 a0) Char)
    The type variables `k0', `a0' are ambiguous
    ...

At first glance it seemed to work. but what happened with the empty list? Wasn't it supposed to throw the error we specified in our definition? What happened is that the type of empty is polymorphic. And even though we defined

	> type Output (Map key value) key = value

there is no way for us or ghc to know that somebody else will not define something else

	> type Output (Map () value) Char= Int

We could just ignore this since it only happens with the empty list and querying an empty list is stupid anyway. However, since other functions such as insert can be done on an empty list and will work exactly the same we need to fix this.

The fix I have come up with is to expand the instance to include anypossible key and then after the instance is already matched to ensure that anykey is infact the right key.

	instance (anykey ~ key, Ord key) => Action (Map key value) anykey where
	...

Ok. Now, if we query the empty list our instance will match,

	> empty . 'f'
	*** Exception: no element of this key in map

and the error we specified is raised. The astute reader may have noticed that we effectively prevented any other Action instance to be declared for Map. This issue will be discussed in a later section (hint: closed type families).

## Name collision

One of the really big things about object oriented programming is that names can be reused among different objects and don't need prefixes. So how can we achieve it that two models, say List and Map, who don't know about each other can both declare some size thing that can be used in a third module to act both on List and Map without prefix. Easy size has to be the same thing in both cases. Suppose we have a central module where all letters have types as follows.

	data T_a = T_a
	data T_b = T_b
	data T_c = T_c
	...

Now we can define size in both List and Map

	size = (T_s,T_i,T_z,T_e)

And to use any Action defined for size, you can use either the size in Map or the size in List. The definition of size can be automatized using template haskell. We also usually provide a type synonym and wrap the tuple to make our life later on more easy.

	newtype Method a = Method a -- in central module
	
	type Size_ = Method (L_l,L_e,L_n,L_g,L_t,L_h)
	size = Method (T_s,T_i,T_z,T_e) :: Size_

A still unresolved issue is how to automatically import only one of the two size variables. As of now you just have to hide one explicitly.

## closed type families

In ghc 7.7 closed type families were Introduced. They let us have general cases and more specific cases which overlap. in order for this to work we have to make output a separate type family that is not associated with Action.

	type family Output object action

Now we can tell Output to use a closed typefamily in the case of object being Map.

	type instance Output (Map a b) action = OutputMap
	
	type family OutputMap where	
		OutputMap (Map k v) (Method a) = Int
		OutputMap (Map k v) [k' := v'] = Map k v
		OutputMap (Map k v) k' = v

Now, additionally there is some additional context shuffling . But after this is all done you do indeed end up with a working library that will enable you to write use maps exactly as in the first example. 

## WIP

I am working on code which will follow soon.

## Open World 2 and numbers

The problem with numbers and typefamilies is outlined on
[stackoverflow](http://stackoverflow.com/questions/22389648/overlapping-incoherent-closed-type-families)

Depending on the popularity of this library I think I might have a solution for this problem that will leave haskell98/2010 compatibility intact and only needs a some changes in base.

# changes

Since 1.0

* Change from type families for output to multiparameter typeclasses. This helps to avoid some ambiguity errors.

# object

Object oriented programming for haskell.
For more information you can also look at the *Object.Example* file.

You should be able to:
	cabal install Object
if you have >= ghc-7.8

The API is also on the gh-pages branch or on (http://yokto.github.io/Object/)

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

	class Action object action output where
		(.) :: object -> action -> output

The rest of this page will be concerned with working out some of the details (mostly ambiguous types). I also want to stress here that I want *as much as possible to work without type signatures*.

## Open world 1 and more type families

Ok so let us first implement the lookup since this is the easiest. This lets us get a value by specifying a key.

	instance (Ord key) => Action (Map key value) key value where
		map . key = fromMaybe (error "no element of this key in map") $ lookup key map

Let's test if it works

	> fromList [('f',1)] . 'f'
	No instance for (Action (Map Char a0) Char output0)
		arising from a use of `.'
	The type variables `output0', `a0' are ambiguous

The compiler can't figure out that we want the instance where 'output' is 'value'. You can fix almost any ambiguity error by making your instance more polimorphic and make sure it's correct later through an equality constraint.

	instance (key'~key, value'~value, Ord key) => Action (Map key value) key' value' where
		map . key = fromMaybe (error "no element of this key in map") $ lookup key map

Now it works.

	>fromList [('f',1)] . 'f'
	1
	> empty . 'f'
	*** Exception: no element of this key in map

and the error we specified is raised.

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

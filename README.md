# object

Object oriented programming for haskell

## Idea

The idea is to be able to use multiparameter typeclasses to create a dot operator which can access certain properties of the object/datatype. We give a simple example of the kind of thing that we want to be able to write.

	> import Data.Map
	> let m = empty . [ 'f' := Just 1, 'o' := Just 2, 'o' := Nothing ]
	> m
	fromList [('f',Just 1),('o',Nothing)]
	> m . 'f'
	Just 1
	> m . length
	2

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

The fix I have come up with is to add a type family called Restrict and add it to the definition of Action

	type family RestrictAction object action
	class (RestrictAction object action ~ (object,action)) => Action object action where
	...

Now let's add a restriction for Map and I will explain afterwards how it works.

	type instance RestrictAction (Map key value) key' = (Map key value, key)

Ok. Now, if we query the empty list, the type checker will look up Action and find that object and action have to match RestrictAction of object and action. They have the following types

	empty :: Map key value
	'f' :: Char

so 

	RestrictAction (Map key value) Char = (Map key value, key) ~ (Map key value, Char)

Therefore, key is equal to Char and the Action instance we defined can be used without problem,

	> empty Main.. 'f'
	*** Exception: no element of this key in map

and the error we specified is raised. The astute reader may have noticed that we effectively prevented any other Action instance to be declared for Map. This issue will be discussed in a later section (hint: closed type families).

## Name collision

One of the really big things about object oriented programming is that names can be reused among different objects and don't need prefixes. So how can we achieve it that two models, say List and Map, who don't know about each other can both declare some length thing that can be used in a third module to act both on List and Map without prefix. Easy length has to be the same thing in both cases. Suppose we have a central module where all letters have types as follows.

	data L_a = L_a
	data L_b = L_b
	data L_c = L_c
	...

Now we can define length in both List and Map

	length = (L_l,L_e,L_n,L_g,L_t,L_h)

And to use any Action defined for length, you can use either the length in Map or the length in List. The definition of length can be automatized using template haskell. We also usually provide a type synonym and wrap the tuple to make our life later on more easy.

	newtype Method a = Method a -- in central module
	
	type Length = (L_l,L_e,L_n,L_g,L_t,L_h)
	length = Method Length

A still unresolved issue is how to automatically import only one of the two length variables. As of now you just have to hide one explicitly.

## closed type families

In ghc 7.7 closed type families were Introduced. They let us have general cases and more specific cases which overlap. So let's try it. Instead of the RestrictAction instance from above we use this.

	type instance RestrictAction (Map key value) action = RestrictMap (Map key value) action
	
	type family RestrictMap (Map key value) action where
		RestrictMap (Map k v) (Method a) = (Map k v, Method a) -- this is why we used the wraper
		RestrictMap (Map k v) [k' := v'] = (Map k v,[k := v])
		RestrictMap (Map k v) k' = (Map k v,k)

Now, additionally there is some context shuffling and we have to take Output away into a separate type family so that the same closed type family procedure can be applied to it (these might all be just missing deductions in ghc). But after this is all done you do indeed end up with a working library that will enable you to write use maps exactly as in the first example. 

## WIP

I am working on code which will follow soon.

## Open World 2 and numbers

The problem with numbers and typefamilies is outlined on
[stackoverflow](http://stackoverflow.com/questions/22389648/overlapping-incoherent-closed-type-families)

Depending on the popularity of this library I think I might have a solution for this problem that will leave haskell98/2010 compatibility intact and only needs a some changes in base.
---
layout: post
title: "Time Traveling Recursion Schemes"
categories:
  - haskell
  - recursion
---

In [Practical Recursion
Schemes](https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29)
I talked about *recursion schemes*, describing them as elegant and useful
patterns for expressing general computation.  In that article I introduced a
number of things relevant to working with the
[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
package in Haskell.

In particular, I went over:

* factoring the recursion out of recursive types using base functors and a
  fixed-point wrapper
* the 'Foldable' and 'Unfoldable' typeclasses corresponding to recursive and
  corecursive data types, plus their 'project' and 'embed' functions
  respectively
* the 'Base' type family that maps recursive types to their base functors
* some of the most common and useful recursion schemes: *cata*, *ana*, *para*,
  and *hylo*.

In [A Tour of Some Useful Recursive
Types](https://medium.com/@jaredtobin/a-tour-of-some-useful-recursive-types-8fa8e423b5b9)
I went into further detail on 'Fix', 'Free', and 'Cofree' - three higher-kinded
recursive types that are useful for encoding programs defined by some
underlying instruction set.

I've also posted a couple of minor notes - I described the *apo* scheme in
[Sorting Slower With Style](http://jtobin.ca/sorting-slower-with-style) (as
well as how to use *recursion-schemes* with regular Haskell lists) and chatted
about monadic versions of the various schemes in [Monadic Recursion
Schemes](http://jtobin.ca/monadic-recursion-schemes).

Here I want to clue up this whole recursion series by briefly talking about two
other recursion schemes - *histo* and *futu* - that work by looking at the past
or future of the recursion respectively.

Here's a little preamble for the examples to come:

``` haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable
```

### Histomorphisms

Histomorphisms are terrifically simple - they just give you access to arbitrary
previously-computed values of the recursion at any given point (its *history*,
hence the namesake).  They're perfectly suited to dynamic programming problems,
or anything where you might need to re-use intermediate computations later.

*Histo* needs a data structure to store the history of the recursion in. The
the natural choice there is 'Cofree', which allows one to annotate recursive
types with arbitrary metadata.  Brian McKenna wrote [a great
article](http://brianmckenna.org/blog/type_annotation_cofree) on making
practical use of these kind of annotations awhile back.

But yeah, histomorphisms are very easy to use.  Check out the following
function that returns all the odd-indexed elements of a list:

``` haskell
oddIndices :: [a] -> [a]
oddIndices = histo $ \case
  Nil                           -> []
  Cons h (_ :< Nil)             -> [h]
  Cons h (_ :< Cons _ (t :< _)) -> h:t
```

The value to the left of a ':<' constructor is an *annotation* provided by
'Cofree', and the value to right is the (similarly annotated) next step of the
recursion.  The annotations at any point are the previously computed values of
the recursion corresponding to that point.

So in the case above, we're just grabbing some elements from the input list and
ignoring others.  The algebra is saying:

* if the input list is empty, return an empty list
* if the input list has only one element, return that one-element list
* if the input list has at least two elements, return the list built by
  cons-ing the first element together with the list computed two steps ago

The list computed two steps ago is available as the annotation on the
constructor two steps down - I've matched it as 't' in the last line of the
above example.  Like *cata*, *histo* works from the bottom-up.

A function that computes even indices is similar:

``` haskell
evenIndices :: [a] -> [a]
evenIndices = histo $ \case
  Nil                           -> []
  Cons _ (_ :< Nil)             -> []
  Cons _ (_ :< Cons h (t :< _)) -> h:t
```

### Futumorphisms

Like histomorphisms, futumorphisms are also simple.  They give you access to
a particular computed part of the recursion at any given point.

However I'll concede that the perceived simplicity probably comes with
experience, and there is likely some conceptual weirdness to be found here.
Just as *histo* gives you access to previously-computed values, *futu* gives
you access to values that the recursion will compute in the future.

![wat](/images/lions-wat.gif "wat")

So yeah, that sounds crazy.  But the reality is more mundane, if you're
familiar with the underlying concepts.

For *histo*, the recursion proceeds from the bottom up.  At each point, the
part of the recursive type you're working with is annotated with the value of
the recursion at that point (using 'Cofree'), so you can always just reach back
and grab it for use in the present.

With *futu*, the recursion proceeds from the top down.  At each point, you
construct an expression that can make use of a value to be supplied later.
When the value does become available, you can use it to evaluate the
expression.

A histomorphism makes use of 'Cofree' to do its annotation, so it should be no
surprise that a futumorphism uses the dual structure - 'Free' - to create its
expressions.  The well-known 'free monad' is [tremendously
useful](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)
for working with small embedded languages.  I also wrote about 'Free' in the
same article mentioned previously, so I'll [link it
again](https://medium.com/@jaredtobin/a-tour-of-some-useful-recursive-types-8fa8e423b5b9)
in case you want to refer to it.

As an example, we'll use *futu* to implement the same two functions that we did
for *histo*.  First, the function that grabs all odd-indexed elements:

``` haskell
oddIndicesF :: [a] -> [a]
oddIndicesF = futu coalg where
  coalg list = case project list of
    Nil      -> Nil
    Cons x s -> Cons x $ do
      return $ case project s of
        Nil      -> s
        Cons _ t -> t
```

The coalgebra is saying the following:

*  if the input list is empty, return an empty list
*  if the input list has at least one element, construct an expression that
   will use a value to be supplied later.
*  if the supplied value turns out to be empty, return the one-element list.
*  if the supplied value turns out to have at least one more element, return the
   list constructed by skipping it, and using the value from another step in
   the future.

You can write that function more concisely, and indeed
[HLint](https://hackage.haskell.org/package/hlint) will complain at you for
writing it as I've done above.  But I think this one makes it clear what parts
are happening based on values to be supplied in the future.  Namely, anything
that occurs after 'do'.

It's kind of cool - you Enter The Monad™ and can suddenly work with values that
don't exist yet, while treating them as if they do.

Here's *futu*-implemented 'evenIndices' for good measure:

``` haskell
evenIndicesF :: [a] -> [a]
evenIndicesF = futu coalg where
  coalg list = case project list of
    Nil      -> Nil
    Cons _ s -> case project s of
      Nil -> Nil
      Cons h t -> Cons h $ return t
```

Sort of a neat feature is that 'Free' part of the coalgebra can be written
a little cleaner if you have 'Free'-encoded embedded language terms floating
around.  Here are a couple of such terms, plus a 'twiddle' function that uses
them to permute elements of an input list as they're encountered:

``` haskell
nil :: Free (Prim [a]) b
nil = liftF Nil

cons :: a -> b -> Free (Prim [a]) b
cons h t = liftF (Cons h t)

twiddle :: [a] -> [a]
twiddle = futu coalg where
  coalg r = case project r of
    Nil      -> Nil
    Cons x l -> case project l of
      Nil      -> Cons x nil
      Cons h t -> Cons h $ cons x t
```

If you've been looking to twiddle elements of a recursive type then you've
found a classy way to do it:

```
> take 20 $ twiddle [1..]
[2,1,4,3,6,5,8,7,10,9,12,11,14,13,16,15,18,17,20,19]
```

Enjoy!  You can find the code from this article in this
[gist](https://gist.github.com/jtobin/bbb2070f6a63956401b3).


---
layout: post
title: "Sorting with Style"
categories:
  - haskell
  - recursion
---

[Merge sort](https://en.wikipedia.org/wiki/Merge_sort) is a famous
comparison-based sorting algorithm that starts by first recursively dividing a
collection of orderable elements into smaller subcollections, and then finishes
by recursively sorting and merging the smaller subcollections together to
reconstruct the (now sorted) original.

A clear implementation of mergesort should by definition be as faithful to that
high-level description as possible.  We can get pretty close to that using the
whole [recursion schemes](http://jtobin.ca/practical-recursion-schemes)
business that I've talked about in the past.  Near the end of that article I
briefly mentioned the idea of implementing mergesort via a
[hylomorphism](https://en.wikipedia.org/wiki/Hylomorphism_(computer_science)),
and here I just want to elaborate on that a little.

Start with a collection of orderable elements.  We can divide the collection
into a bunch of smaller collections by using a binary tree:

``` haskell
{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Foldable (hylo)
import Data.List.Ordered (merge)

data Tree a r =
    Empty
  | Leaf a
  | Node r r
  deriving Functor
```

The idea is that each node in the tree holds two subtrees, each of which
contains half of the remaining elements.  We can build a tree like this from a
collection - say, a basic Haskell list.  The following `unfolder` function
defines what part of a tree to build for any corresponding part of a list:

``` haskell
unfolder []  = Empty
unfolder [x] = Leaf x
unfolder xs  = Node l r where
  (l, r) = splitAt (length xs `div` 2) xs
```

On the other hand, we can also collapse an existing tree back into a list.  The
following `folder` function defines how to collapse any given part of a tree
into the corresponding part of a list; again we just pattern match on whatever
part of the tree we're looking at, and construct the complementary list:

``` haskell
folder Empty      = []
folder (Leaf x)   = [x]
folder (Node l r) = merge l r
```

Now to sort a list we can just glue these instructions together using
a hylomorphism:

``` haskell
mergesort :: Ord a => [a] -> [a]
mergesort = hylo folder unfolder
```

And it works just like you'd expect:

```
> mergesort [1,10,3,4,5]
[1,3,4,5,10]
> mergesort "aloha"
"aahlo"
> mergesort [True, False, False, True, False]
[False, False, False, True, True]
```

Pretty concise!

The code is eminently clean and faithful to the high-level algorithm
description: first recursively divide a collection into smaller subcollections
- via a binary tree and `unfolder` - and then recursively sort and merge the
subcollections to reconstruct the (now sorted) original one - via `folder`.

A version of this post originally appeared on the [Fugue
blog](https://blog.fugue.co/).

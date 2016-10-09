---
layout: post
title: "A Tour of Some Useful Recursive Types"
categories:
  - haskell
  - recursion
---

(This article is also published at
[Medium](https://medium.com/@jaredtobin/a-tour-of-some-useful-recursive-types-8fa8e423b5b9))

I'm presently at [NIPS](https://nips.cc/) and so felt like writing about some
appropriate machine learning topic, but along the way I wound up talking about
parameterized recursive types, and here we are.  Enjoy!

One starts to see common 'shapes' in algebraic data types after working with
them for a while.  Take the natural numbers and a standard linked list, for
example:

``` haskell
data Natural =
    One
  | Succ Natural

data List a =
    Empty
  | Cons a (List a)
```

These are similar in some sense.  There are some differences - a list has an
additional type parameter, and each recursive point in the list is tagged with
a value of that type - but the nature of the recursion in each is the same.
There is a single recursive point wrapped up in a single constructor, plus a
single base case.

Consider a recursive type that is parameterized by a functor with kind '\* ->
\*', such that the kind of the resulting type is something like '(\* -> \*) ->
\*' or '(\* -> \*) -> \* -> \*' or so on.  It's interesting to look at the
'shapes' of some useful types like this and see what kind of similarities and
differences in recursive structure that we can find.

In this article we'll look at three such recursive types: 'Fix', 'Free', and
'Cofree'.  I'll demonstrate that each can be viewed as a kind of program
parameterized by some underlying instruction set.

## Fix

To start, let's review the famous fixed-point type 'Fix'.  I've talked about it
[before](practical-recursion-schemes), but will go into a bit more detail here.

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecideableInstances #-}

newtype Fix f = Fix (f (Fix f))

deriving instance (Show (f (Fix f))) => Show (Fix f)
```

Note: I'll omit interpreter output for examples throughout this article, but
feel free to try the code yourself in GHCi.  I'll post some gists at the
bottom.  The above code block also contains some pragmas that you can ignore;
they're just there to help GHC derive some instances for us.

Anyway.  'Fix' is in some sense a template recursive structure.  It relies on
some underlying functor 'f' to define the scope of recursion that you can
expect a value with type 'Fix f' to support.  There is the degenerate constant
case, for example, which supports *no* recursion:

``` haskell
data DegenerateF r = DegenerateF
  deriving (Functor, Show)

type Degenerate = Fix DegenerateF

degenerate :: Degenerate
degenerate = Fix DegenerateF
```

Then you have the case like the one below, where *only* an infinitely recursive
value exists:

``` haskell
newtype InfiniteF r = InfiniteF r
  deriving (Functor, Show)

type Infinite = Fix InfiniteF

infinite :: Infinite
infinite = Fix (InfiniteF infinite)
```

But in practice you'll have something in between; a type with at least one
recursive point or 'running' case and also at least one base or 'terminating'
case.  Take the natural numbers, for example:

``` haskell
data NatF r =
    OneF
  | SuccF r
  deriving (Functor, Show)

type Nat = Fix NatF

one :: Nat
one = Fix OneF

succ :: Nat -> Nat
succ = Fix . SuccF
```

Here 'NatF' provides both a 'running' case - 'SuccF' - and a 'terminating' case
in - 'OneF'.  'Fix' just lets 'NatF' do whatever it wants, having no say of its
own about termination.  In fact, we could have defined 'Fix' like this:

``` haskell
data Program f = Running (f (Program f))
```

Indeed, you can think of 'Fix' as defining a program that runs until 'f'
decides to terminate.  In turn, you can think of 'f' as an instruction
set for the program.  The whole shebang of 'Fix f' may only terminate if 'f'
contains a terminating instruction.

Here's a simple set of instructions, for example:

``` haskell
data Instruction r =
    Increment r
  | Decrement r
  | Terminate
  deriving (Functor, Show)

increment :: Program Instruction -> Program Instruction
increment = Running . Increment

decrement :: Program Instruction -> Program Instruction
decrement = Running . Decrement

terminate :: Program Instruction
terminate = Running Terminate
```

And we can write a sort of stack-based program like so:

``` haskell
program :: Program Instruction
program =
    increment
  . increment
  . decrement
  $ terminate
```

### Richness of 'Fix'

It's worthwhile to review two functions that are useful for working with 'Fix',
unimaginatively named 'fix' and 'unfix':

``` haskell
fix :: f (Fix f) -> Fix f
fix = Fix

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f
```

You can think of them like so: 'fix' embeds a value of type 'f' into a
recursive structure by adding a new layer of recursion, while 'unfix' projects
a value of type 'f' out of a recursive structure by peeling back a layer of
recursion.

This is a pretty rich recursive structure - we have a guarantee that we can
*always* embed into or project out of something with type 'Fix f', no matter
what 'f' is.

## Free

Next up is 'Free', which is really just 'Fix' with some added structure.  It is
defined as follows:

``` haskell
data Free f a =
    Free (f (Free f a))
  | Pure a
  deriving Functor

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
```

The 'Free' constructor has an analogous definition to the 'Fix' constructor,
meaning we can use 'Free' to implement the same things we did previously.  Here
are the natural numbers redux, for example:

``` haskell
type NatFree = Free NatF

oneFree :: NatFree a
oneFree = Free OneF

succFree :: NatFree a -> NatFree a
succFree = Free . SuccF
```

There's also another branch here called 'Pure', though, that just bluntly wraps
a value of type 'a', and has nothing to do with the parameter 'f'.  This has an
interesting consequence: it means that 'Free' can have an opinion of its own
about termination, regardless about what 'f' might decree:

``` haskell
type NotSoInfinite = Free InfiniteF

notSoInfinite :: NotSoInfinite ()
notSoInfinite = Free (InfiniteF (Free (InfiniteF (Pure ()))))
```

(Note that here I've returned the value of type unit when terminating under the
'Pure' branch, but you could pick whatever else you'd like.)

You'll recall that 'InfiniteF' provides no terminating instruction,
and left to its own devices will just recurse endlessly.

So: instead of being forced to choose a branch of the underlying functor to
recurse on, 'Free' can just bail out on a whim and return some value wrapped up
in 'Pure'.  We could have defined the whole type like this:

``` haskell
data Program f a =
    Running (f (Program f a))
  | Terminated a
  deriving Functor
```

Again, it's 'Fix' with more structure.  It's a program that runs until 'f'
decides to terminate, *or* that terminates and returns a value of type 'a'

As a quick illustration, take our simple stack-based instruction set again.  We
can define the following embedded language terms:

``` haskell
increment :: Program Instruction a -> Program Instruction a
increment = Running . Increment

decrement :: Program Instruction a -> Program Instruction a
decrement = Running . Decrement

terminate :: Program Instruction a
terminate = Running Terminate

sigkill :: Program f Int
sigkill = Terminated 1
```

So note that 'sigkill' is independent of whatever instruction set we're working
with.  We can thus write another simple program like before, except this time
have 'sigkill' terminate it:

``` haskell
program :: Program Instruction Int
program =
    increment
  . increment
  . decrement
  $ sigkill
```

### Richness of 'Free'

Try to define the equivalent versions of 'fix' and 'unfix' for 'Free'.  The
equivalent to 'fix' is easy:

``` haskell
free :: f (Free f a) -> Free f a
free = Free
```

You'll hit a wall, though, if you want to implement the (total) analogue to
'unfix'.  One wants a function of type 'Free f a -> f (Free f a)', but the
existence of the 'Pure' branch makes this impossible to implement totally.  In
general there is not going to be an 'f' to pluck out:

``` haskell
unfree :: Free f a -> f (Free f a)
unfree (Free f) = f
unfree (Pure a) = error "kaboom"
```

The recursion provided by 'Free' is thus a little less rich than that provided
by 'Fix'.  With 'Fix' one can *always* project a value out of its recursive
structure - but that's not the case with 'Free'.

It's well-known that 'Free' is monadic, and indeed it's usually called the
'[free
monad](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)'.
The namesake 'free' comes from an algebraic definition; roughly, a free 'foo'
is a 'foo' that satisfies the minimum possible constraints to make it a 'foo',
and nothing else.  Check out the
[slides](https://drive.google.com/file/d/0B51SFgxqMDS-NDBOX0ZDdW52dEE/edit)
from Dan Piponi's excellent talk from Bayhac a few years back for a deeper dive
on algebraic freeness.

## Cofree

'Cofree' is also like 'Fix', but again with some extra structure.  It can be
defined as follows:

``` haskell
data Cofree f a = Cofree a (f (Cofree f a))
  deriving Functor

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)
```

Again, part of the definition - the second field of the 'Cofree' constructor -
looks just like 'Fix'.  So predictably we can do a redux-redux of the natural
numbers using 'Cofree':

``` haskell
type NatCofree = Cofree NatF

oneCofree :: NatCofree ()
oneCofree = Cofree () OneF

succFree :: NatCofree () -> NatCofree ()
succFree f = Cofree () (SuccF f)
```

(Note that here I've again used unit to fill in the first field - you could
of course choose whatever you'd like.)

This looks a lot like 'Free', and in fact it's the *categorical dual* of
'Free'.  Whereas 'Free' is a sum type with two *branches*, 'Cofree' is a
product type with two *fields*.  In the case of 'Free', we could have a program
that either runs an instruction from a set 'f', *or* terminates with a value
having type 'a'.  In the case of 'Cofree', we have a program that runs an
instruction from a set 'f' *and* returns a value of type 'a'.

A 'Free' value thus contains at most one recursive point wrapping the value
with type 'a', while a 'Cofree' value contains potentially infinite recursive
points - each one of which is tagged with a value of type 'a'.

Rolling with the 'Program' analogy, we could have written this alternate
definition for 'Cofree':

``` haskell
data Program f a = Program {
    annotation :: a
  , running    :: f (Program f a)
  } deriving Show
```

A 'Cofree' value is thus a program in which every instruction is annotated with
a value of type 'a'.  This means that, unlike 'Free', it can't have its own
opinion on termination.  Like 'Fix', it has to let 'f' decide how to do that.

We'll use the stack-based instruction set example to wrap up.  Here we can
annotate instructions with progress about how many instructions remain to
execute.  First our new embedded language terms:

``` haskell
increment :: Program Instruction Int -> Program Instruction Int
increment p = Program (remaining p) (Increment p)

decrement :: Program Instruction Int -> Program Instruction Int
decrement p = Program (remaining p) (Decrement p)

terminate :: Program Instruction Int
terminate = Program 0 Terminate
```

Notice that two of these terms use a helper function 'remaining' that counts
the number of instructions left in the program.  It's defined as follows:

``` haskell
remaining :: Program Instruction Int -> Int
remaining = loop where
  loop (Program a f) = case f of
    Increment p -> succ (loop p)
    Decrement p -> succ (loop p)
    Terminate   -> succ a
```

And we can write our toy program like so:

``` haskell
program :: Program Instruction Int
program =
    increment
  . increment
  . decrement
  $ terminate
```

Evaluate it in GHCi to see what the resulting value looks like.

### Richness of 'Cofree'

If you try and implement the 'fix' and 'unfix' analogues for 'Cofree' you'll
rapidly infer that we have the opposite situation to 'Free' here.  Implementing
the 'unfix' analogue is easy:

``` haskell
uncofree :: Cofree f a -> f (Cofree f a)
uncofree (Cofree _ f) = f
```

But implementing a total function corresponding to 'fix' is impossible - we
can't just come up with something of arbitrary type 'a' to tag an instruction
'f' with, so, like before, we can't do any better than define something
partially:

```
cofree :: f (Cofree f a) -> Cofree f a
cofree f = Cofree (error "kaboom") f
```

Just as how 'Free' forms a monad, 'Cofree' forms a comonad.  It's thus known as
the 'cofree comonad', though I can't claim to really have any idea what the
algebraic notion of 'cofreeness' captures, exactly.

## Wrapping Up

So: 'Fix', 'Free', and 'Cofree' all share a similar sort of recursive structure
that make them useful for encoding programs, given some instruction set.  And
while their definitions are similar, 'Fix' supports the richest recursion of
the three in some sense - it can both 'embed' things into *and* 'project'
things out of its recursive structure, while 'Free' supports only embedding and
'Cofree' supports only projecting.

This has a practical implication: it means one can't make use of certain
recursion schemes for 'Free' and 'Cofree' in the same way that one can for
'Fix'.  There do exist analogues, but they're sort of out-of-scope for this
post.

I haven't actually mentioned any truly practical uses of 'Free' and 'Cofree'
here, but they're wonderful things to keep in your toolkit if you're doing any
work with embedded languages, and I'll likely write more about them in the
future.  In the meantime, Dave Laing wrote an excellent [series of
posts](http://dlaing.org/cofun/posts/free_and_cofree.html) on 'Free' and
'Cofree' that are more than worth reading.  They go into much more interesting
detail than I've done here - in particular he details a nice pairing that
exists between 'Free' and 'Cofree' (also
[discussed](http://blog.sigfpe.com/2014/05/cofree-meets-free.html) by Dan
Piponi), plus a whack of examples.

You can also find industrial-strength infrastructure for both 'Free' and
'Cofree' in Edward Kmett's excellent
[free](https://hackage.haskell.org/package/free) library, and for 'Fix' in
[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes).

I've dumped the code for this article into a few gists.
[Here's](https://gist.github.com/jtobin/c95efd75bd8b894d10c0) one of everything
excluding the running 'Program' examples, and here are the corresponding
'Program' examples for the
[Fix](https://gist.github.com/jtobin/0b173dae5bdc46cf3fa3),
[Free](https://gist.github.com/jtobin/0a609ae3f4704fafc611), and
[Cofree](https://gist.github.com/jtobin/ba992310771bd499e457) cases
respectively.

Thanks to Fredrik Olsen for review and great feedback.

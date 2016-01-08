---
layout: post
title: "Yo Dawg We Heard You Like Derivatives"
categories:
  - math
  - language-engineering
  - haskell
---

I noticed [this
article](http://h2.jaguarpaw.co.uk/posts/symbolic-expressions-can-be-automatically-differentiated/)
by Tom Ellis today that provides an excellent 'demystified' introduction to
[automatic differentiation](http://alexey.radul.name/ideas/2013/introduction-to-automatic-differentiation/).  His exposition is exceptionally clear and simple.

Hopefully not in the spirit of re-mystifying things too much, I wanted to
demonstrate that this kind of forward-mode automatic differentiation can be
implemented using a paramorphism, which cleans up the various `let` statements
found in Tom's version (at the expense of more pattern matching).

Let me first duplicate his setup using the standard [recursion
scheme](http://jtobin.ca/practical-recursion-schemes/) machinery:

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Functor.Foldable

data ExprF r =
    VarF
  | ZeroF
  | OneF
  | NegateF r
  | SumF r r
  | ProductF r r
  | ExpF r
  deriving (Show, Functor)

type Expr = Fix ExprF
```

Since my expression type uses a fixed-point wrapper I'll define my own
embedded language terms to get around it:

``` haskell
var :: Expr
var = Fix VarF

zero :: Expr
zero = Fix ZeroF

one :: Expr
one = Fix OneF

neg :: Expr -> Expr
neg x = Fix (NegateF x)

add :: Expr -> Expr -> Expr
add a b = Fix (SumF a b)

prod :: Expr -> Expr -> Expr
prod a b = Fix (ProductF a b)

e :: Expr -> Expr
e x = Fix (ExpF x)
```

To implement a corresponding `eval` function we can use a catamorphism:

``` haskell
eval :: Double -> Expr -> Double
eval x = cata $ \case
  VarF         -> x
  ZeroF        -> 0
  OneF         -> 1
  NegateF a    -> negate a
  SumF a b     -> a + b
  ProductF a b -> a * b
  ExpF a       -> exp a
```

Very clear.  We just match things mechanically.

Now, symbolic differentiation.  If you refer to the original `diff` function
you'll notice that in cases like `Product` or `Expr` there are uses of both an
original expression and also its derivative.  This can be captured simply by a
paramorphism:

``` haskell
diff :: Expr -> Expr
diff = para $ \case
  VarF                     -> one
  ZeroF                    -> zero
  OneF                     -> zero
  NegateF (_, x')          -> neg x'
  SumF (_, x') (_, y')     -> add x' y'
  ProductF (x, x') (y, y') -> add (prod x y') (prod x' y)
  ExpF (x, x')             -> prod (e x) x'
```

Here the primes indicate derivatives in the usual fashion, and the standard
rules of differentiation are self-explanatory.

For automatic differentiation we just do the same thing, except we're also also
going to lug around the evaluated function value itself at each point and
evaluate to doubles instead of other expressions.

It's worth noting here: why doubles?  Because the expression type that we've
defined has no notion of sharing, and thus the expressions will blow up à la
`diff` (to see what I mean, try printing the analogue of `diff bigExpression`
in GHCi).  This could probably be mitigated by [incorporating sharing into the
embedded language](http://jtobin.ca/sharing-in-haskell-edsls/) in some way, but
that's a topic for another post.

Anyway, another paramorphism will do the trick:

``` haskell
ad :: Double -> Expr -> (Double, Double)
ad x = para $ \case
  VarF                                   -> (x, 1)
  ZeroF                                  -> (0, 0)
  OneF                                   -> (1, 0)
  NegateF (_, (ex, ed))                  -> (negate ex, negate ed)
  SumF (_, (ex, ed)) (_, (ex', ed'))     -> (ex + ex', ed + ed')
  ProductF (_, (ex, ed)) (_, (ex', ed')) -> (ex * ex', ex * ed' + ed * ex')
  ExpF (_, (ex, ed))                     -> (exp ex, exp ex * ed)
```

Take a look at the pairs to the right of the pattern matches; the first element
in each is just the corresponding term from `eval`, and the second is just the
corresponding term from `diff` (made 'Double'-friendly).  The paramorphism
gives us access to all the terms we need, and we can avoid a lot of work on
the right-hand side by doing some more pattern matching on the left.

Some sanity checks to make sure that these functions match up with Tom's:

```
*Main> map (snd . (`ad` testSmall)) [0.0009, 1.0, 1.0001]
[0.12254834896191881,1.0,1.0003000600100016]
*Main> map (snd . (`ad` testBig)) [0.00009, 1.0, 1.00001]
[3.2478565715996756e-6,1.0,1.0100754777229357]
```


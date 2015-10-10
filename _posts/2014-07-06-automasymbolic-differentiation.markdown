---
layout: post
title: "Automasymbolic Differentiation"
categories:
  - language-engineering
  - haskell
---

Automatic differentiation is one of those things that's famous for not being as
famous as it should be (uh..).  It's useful, it's convenient, and yet fewer
know about it than one would think.

[This
article](http://alexey.radul.name/ideas/2013/introduction-to-automatic-differentiation/)
(by one of the guys working on
[Venture](http://probcomp.csail.mit.edu/venture/)) is the single best
introduction you'll probably find to AD, anywhere.  It gives a wonderful
introduction to the basics, the subtleties, and the gotchas of the subject.
You should read it.  I'm going to assume you have.

In particular, you should note this part:

> [computing gradients] can be done — the method is called *reverse mode* — but
> it introduces both code complexity and runtime cost in the form of managing
> this storage (traditionally called the “tape”). In particular, the space
> requirements of raw reverse mode are proportional to the *runtime* of f.

In some applications this can be somewhat inconvenient - picture iterative
gradient-based sampling algorithms like [Hamiltonian Monte Carlo](http://arxiv.org/pdf/1206.1901.pdf), its famous auto-tuning version
[NUTS](http://arxiv.org/abs/1111.4246), and [Riemannian manifold variants](http://arxiv.org/pdf/1011.0057.pdf).  [Tom](http://bayeshive.com/)
noted in [this reddit thread](http://www.reddit.com/r/haskell/comments/1odmk1/backpropogation_steepest_descent_and_automatic/ccrc6pg)
that symbolic differentiation - which doesn't need to deal with tapes and
infinitesimal-tracking - can often be orders of magnitude faster than AD for
this kind of problem.  When running these algos we calculate gradients many,
many times, and the additional runtime cost of the reverse-mode AD dance can
add up.

An interesting question is whether or not this can this be mitigated
at all, and to what degree.  In particular: can we use automatic
differentiation to *implement* efficient symbolic differentiation?  Here's a
possible attack plan:

* use an existing automatic differentiation implementation to calculate the
  gradient for some target function at a point
* capture the symbolic expression for the gradient and optimize it by
  eliminating common subexpressions or whatnot
* reify that expression as a function that we can evaluate for any input
* voila, (more) efficient gradient

Ed Kmett's ['ad' library](http://hackage.haskell.org/package/ad) is the
best automatic differentiation library I know of, in terms of its balance
between power and accessibility.  It can be used on arbitrary Haskell types
that are instances of the `Num` typeclass and can carry out automatic
differentiation via a number of modes, so it's very easy to get started with.
Rather than rolling my own AD implementation to try to do this sort of thing,
I'd much rather use his.

Start with a really basic language.  In practice we'd be interested in
working with more expressive things, but this is sort of the minimal
interesting language capable of illustrating the problem:

``` haskell
import Numeric.AD

data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Eq, Show)

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1 = Add e0 e1
  e0 - e1 = Sub e0 e1
  e0 * e1 = Mul e0 e1
```

We can already use Kmett's ad library on these expressions to generate symbolic
expressions.  We just have to write functions we're interested in generically
(using `+`, `-`, and `*`) and then call `diff` or `grad` or whatnot on them
with a concretely-typed argument.  Some examples:

    > :t diff (\x -> 2 * x ^ 2)
    diff (\x -> 2 * x ^ 2) :: Num a => a -> a

    > diff (\x -> 2 * x ^ 2) (Lit 1)
    Mul (Add (Mul (Lit 1) (Lit 1)) (Mul (Lit 1) (Lit 1))) (Lit 2)

    > grad (\[x, y] -> 2 * x ^ 2 + 3 * y) [Lit 1, Lit 2]
    [ Add (Lit 0) (Add (Add (Lit 0) (Mul (Lit 1) (Add (Lit 0) (Mul (Lit 2) (Add
      (Lit 0) (Mul (Lit 1) (Lit 1))))))) (Mul (Lit 1) (Add (Lit 0) (Mul (Lit 2) (Add
      (Lit 0) (Mul (Lit 1) (Lit 1)))))))
    , Add (Lit 0) (Add (Lit 0) (Mul (Lit 3) (Add (Lit 0) (Mul (Lit 1) (Lit 1)))))
    ]

It's really easy to extract a proper derivative/gradient 'function' by
doing something like this:

    > diff (\x -> 2 * x ^ 2) (Var "x")
    Mul (Add (Mul (Var "x") (Lit 1)) (Mul (Lit 1) (Var "x"))) (Lit 2)

and then, given that expression, reifying a direct function for the derivative
by substituting over the captured variable and evaluating everything.  Here's
some initial machinery to handle that:

``` haskell
-- | Close an expression over some variable.
close :: Expr a -> String -> a -> Expr a
close (Add e0 e1) s x = Add (close e0 s x) (close e1 s x)
close (Sub e0 e1) s x = Sub (close e0 s x) (close e1 s x)
close (Mul e0 e1) s x = Mul (close e0 s x) (close e1 s x)

close (Var v) s x
  | v == s    = Lit x
  | otherwise = Var v

close e _ _ = e

-- | Evaluate a closed expression.
eval :: Num a => Expr a -> a
eval (Lit d) = d
eval (Var _) = error "expression not closed"
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
```

So, using this on the example above yields

    > let testExpr = diff (\x -> 2 * x ^ 2) (Var "x")
    > eval . close testExpr "x" $ 1
    2

and it looks like a basic `toDerivative` function for expressions could be
implemented as follows:

``` haskell
-- | Do some roundabout AD.
toDerivative expr = eval . close diffExpr "x" where
  diffExpr = diff expr (Var "x")
```

But that's a no go.  'ad' throws a type error, as presumably we'd be at risk of
perturbation confusion:

    Couldn't match expected type ‘AD
                                    s (Numeric.AD.Internal.Forward.Forward (Expr c))
                                  -> AD s (Numeric.AD.Internal.Forward.Forward (Expr c))’
                with actual type ‘t’
      because type variable ‘s’ would escape its scope
    This (rigid, skolem) type variable is bound by
      a type expected by the context:
        AD s (Numeric.AD.Internal.Forward.Forward (Expr c))
        -> AD s (Numeric.AD.Internal.Forward.Forward (Expr c))
      at ParamBasic.hs:174:14-32
    Relevant bindings include
      diffExpr :: Expr c (bound at ParamBasic.hs:174:3)
      expr :: t (bound at ParamBasic.hs:173:14)
      toDerivative :: t -> c -> c (bound at ParamBasic.hs:173:1)
    In the first argument of ‘diff’, namely ‘expr’
    In the expression: diff expr (Var "x")

Instead, we can use ad's `auto` combinator to write an alternate eval function:

``` haskell
autoEval :: Mode a => String -> Expr (Scalar a) -> a -> a
autoEval x expr = (`go` expr) where
  go _ (Lit d) = auto d
  go v (Var s)
    | s == x    = v
    | otherwise = error "expression not closed"

  go v (Add e0 e1) = go v e0 + go v e1
  go v (Sub e0 e1) = go v e0 - go v e1
  go v (Mul e0 e1) = go v e0 * go v e1
```

and using that, implement a working `toDerivative`:

``` haskell
toDerivative :: Num a => String -> Expr (Expr a) -> Expr a
toDerivative v expr = diff (autoEval v expr)
```

which, though it has a weird-looking type, typechecks and does the trick:

    > let diffExpr = toDerivative "x" (Mul (Lit 2) (Mul (Var "x") (Var "x"))) (Var "x")
    Mul (Add (Mul (Var "x") (Lit 1)) (Mul (Lit 1) (Var "x"))) (Lit 2)

    > eval . close "x" 1 $ diffExpr
    4

So now we have access to a reified AST for a derivative (or gradient), which
can be tweaked and optimized as needed.  Cool.

The available optimizations depend heavily on the underlying language.  For
starters, there's easy and universal stuff like this:

``` haskell
-- | Reduce superfluous expressions.
elimIdent :: (Num a, Eq a) => Expr a -> Expr a
elimIdent (Add (Lit 0) e) = elimIdent e
elimIdent (Add e (Lit 0)) = elimIdent e
elimIdent (Add e0 e1)     = Add (elimIdent e0) (elimIdent e1)

elimIdent (Sub (Lit 0) e) = elimIdent e
elimIdent (Sub e (Lit 0)) = elimIdent e
elimIdent (Sub e0 e1)     = Sub (elimIdent e0) (elimIdent e1)

elimIdent (Mul (Lit 1) e) = elimIdent e
elimIdent (Mul e (Lit 1)) = elimIdent e
elimIdent (Mul e0 e1)     = Mul (elimIdent e0) (elimIdent e1)

elimIdent e = e
```

Which lets us do some work up-front:

    > let e = 2 * Var "x" ^ 2 + Var "x" ^ 4
    Add (Mul (Lit 2) (Mul (Var "x") (Var "x"))) (Mul (Mul (Var "x") (Var "x"))
    (Mul (Var "x") (Var "x")))

    > let ge = toDerivative "x" e
    Add (Mul (Add (Mul (Var "x") (Lit 1)) (Mul (Lit 1) (Var "x"))) (Lit 2))
    (Add (Mul (Mul (Var "x") (Var "x")) (Add (Mul (Var "x") (Lit 1)) (Mul
    (Lit 1) (Var "x")))) (Mul (Add (Mul (Var "x") (Lit 1)) (Mul (Lit 1)
    (Var "x"))) (Mul (Var "x") (Var "x"))))

    > let geOptim = elimIdent ge
    Add (Mul (Add (Var "x") (Var "x")) (Lit 2)) (Add (Mul (Mul (Var "x")
    (Var "x")) (Add (Var "x") (Var "x"))) (Mul (Add (Var "x") (Var "x")) (Mul
    (Var "x") (Var "x"))))

    > eval . close "x" 1 $ geOptim
    8

But there are also some more involved optimizations that can be useful for some
languages.  The basic language I've been using above, for example, has no
explicit support for sharing common subexpressions.  You'll recall from [one of my previous posts](/blog/2014/05/29/sharing-in-haskell-edsls/) that we
have a variety of methods to do that in Haskell EDSLs, including some that
allow sharing to be observed without modifying the underlying language.  We can
use `data-reify`, for example, to observe any implicit sharing in expressions:

    > reifyGraph $ geOptim
    let [(1,AddF 2 6),(6,AddF 7 10),(10,MulF 11 12),(12,MulF 4 4),(11,AddF 4 4),
    (7,MulF 8 9),(9,AddF 4 4),(8,MulF 4 4),(2,MulF 3 5),(5,LitF 2),(3,AddF 4 4),
    (4,VarF "x")] in 1

And even make use of a [handy library](http://hackage.haskell.org/package/data-reify-cse) found on Hackage
for performing common subexpression elimination on graphs returned by
`reifyGraph`:

    > cse <$> reifyGraph geOptim
    let [(5,LitF 2),(1,AddF 2 6),(3,AddF 4 4),(6,AddF 7 10),(2,MulF 3 5),
    (10,MulF 3 8),(8,MulF 4 4),(7,MulF 8 3),(4,VarF "x")] in 1

With an appropriate [graph evaluator](/blog/2014/05/29/sharing-in-haskell-edsls/)
we can cut down the size of the syntax we have to traverse substantially.

Happy automasymbolic differentiating!


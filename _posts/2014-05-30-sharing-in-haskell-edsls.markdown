---
layout: post
title: "Sharing in Haskell EDSLs"
categories:
  - language-engineering
  - haskell
---

Lately I've been trying to do some magic by way of nonstandard interpretations
of abstract syntax.  One of the things that I've managed to grok along the way
has been the problem of *sharing* in deeply-embedded languages.

Here's a simple illustration of the 'vanilla' sharing problem by way of plain
Haskell; a function that computes 2^n:

``` haskell
naiveTree :: (Eq a, Num a, Num b) => a -> a
naiveTree 0 = 1
naiveTree n = naiveTree (n - 1) + naiveTree (n - 1)
```

This naive implementation is a poor way to roll as it is exponentially complex
in n.  Look at how evaluation proceeds for something like `naiveTree 4`:

    > naiveTree 4
    > naiveTree 3 + naiveTree 3
    > naiveTree 2 + naiveTree 2 + naiveTree 2 + naiveTree 2
    > naiveTree 1 + naiveTree 1 + naiveTree 1 + naiveTree 1
      + naiveTree 1 + naiveTree 1 + naiveTree 1 + naiveTree 1
    > naiveTree 0 + naiveTree 0 + naiveTree 0 + naiveTree 0
      + naiveTree 0 + naiveTree 0 + naiveTree 0 + naiveTree 0
      + naiveTree 0 + naiveTree 0 + naiveTree 0 + naiveTree 0
      + naiveTree 0 + naiveTree 0 + naiveTree 0 + naiveTree 0
    > 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
    > 16

Each recursive call doubles the number of function evaluations we need to make.
Don't wait up for `naiveTree 50` to return a value.

A better way to write this function would be:

``` haskell
tree :: (Eq a, Num a, Num b) => a -> a
tree 0 = 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared
```

Here we store solutions to subproblems, and thus avoid having to recompute
things over and over.  Look at how `tree 4` proceeds now:

    > tree 4
    > let shared0 =
          let shared1 =
              let shared2 =
                  let shared3 = 1
                  in  shared3 + shared3
              in  shared2 + shared2
          in  shared1 + shared1
      in  shared0 + shared0
    > let shared0 =
          let shared1 =
              let shared2 = 2
              in  shared2 + shared2
          in  shared1 + shared1
      in  shared0 + shared0
    > let shared0 =
          let shared1 = 4
          in  shared1 + shared1
      in  shared0 + shared0
    > let shared0 = 8
      in  shared0 + shared0
    > 16

You could say that Haskell's `let` syntax enables *sharing* between
computations; using it reduces the complexity of our tree implementation from
$$O(2^n)$$ to $$O(n)$$.  `tree 50` now returns instantly:

    > tree 50
    1125899906842624

So let's move everything over to an abstract syntax setting and see how the
results translate there.

Let's start with a minimalist language, known in some circles as Hutton's
Razor.  While puny, it is sufficiently expressive to illustrate the subtleties
of this whole sharing business:

``` haskell
data Expr =
    Lit Int
  | Add Expr Expr
  deriving (Eq, Ord, Show)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+)         = Add

eval :: Expr -> Int
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
```

I've provided a `Num` instance so that we can conveniently write expressions in
this language.  We can use conventional notation and extract abstract syntax
for free by specifying a particular type signature:

    > 1 + 1 :: Expr
    Add (Lit 1) (Lit 1)

And of course we can use `eval` to evaluate things:

    > eval (1 + 1 :: Expr)
    2

Due to the `Num` instance and the polymorphic definitions of `naiveTree` and
`tree`, these functions will automatically work on our expression type.  Check
them out:

    > naiveTree 2 :: Expr
    Add (Add (Lit 1) (Lit 1)) (Add (Lit 1) (Lit 1))

    > tree 2 :: Expr
    Add (Add (Lit 1) (Lit 1)) (Add (Lit 1) (Lit 1))

Notice there's a quirk here: each of these functions - having wildly different
complexities - yields the same abstract syntax, implying that `tree` is no
more efficient than `naiveTree` when it comes to dealing with this expression
type.  That means..

    > eval (tree 50 :: Expr)
    -- ain't happening

So there is a big problem here: Haskell's `let` syntax doesn't carry its
sharing over to our embedded language.  Equivalently, the embedded language is
*not expressive enough* to represent sharing in its own abstract syntax.

There are a few ways to get around this.

Memoizing Evaluation
--------------------

For some interpretations (like evaluation) we can use a memoization library.
Here we can use `Data.StableMemo` to define a clean and simple evaluator:

``` haskell
import Data.StableMemo

memoEval :: Expr -> Int
memoEval = go where
  go = memo eval
  eval (Lit i)     = i
  eval (Add e0 e1) = go e0 + go e1
```

This will very conveniently handle any grimy details of caching intermediate
computations.  It passes the `tree 50` test just fine:

    > memoEval (tree 50 :: Expr)
    1125899906842624

Some other interpretations are still inefficient; a similar `memoPrint`
function will still dump out a massive syntax tree due to the limited
expressiveness of the embedded language.  The memoizer doesn't really allow us
to *observe* sharing, if we're interested in doing that for some reason.

Observing Implicit Sharing
--------------------------

We can actually use GHC's internal sharing analysis to recover any implicit
sharing present in an embedded expression.  This is the technique introduced by
Andy Gill's [Type Safe Observable Sharing In Haskell](http://www.cs.uu.nl/wiki/pub/Afp/CourseLiterature/Gill-09-TypeSafeReification.pdf)
and implemented in the `data-reify` library on
[Hackage](http://hackage.haskell.org/package/data-reify).  It's as
technically unsafe as it sounds, but in practice has the benefits of being both
relatively benign and minimally intrusive on the existing language.

Here is the extra machinery required to observe implicit sharing in our `Expr`
type:

``` haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Data.Reify hiding (Graph)
import qualified Data.Reify as Reify
import System.IO.Unsafe

data ExprF e =
    LitF Int
  | AddF e e
  deriving (Eq, Ord, Show, Functor)

instance MuRef Expr where
  type DeRef Expr        = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure (LitF v)
```

We need to make `Expr` an instance of the `MuRef` class, which loosely provides
a mapping between the `Expr` and `ExprF` types.  `ExprF` itself is a so-called
'pattern functor', which is a parameterized type in which recursive points are
indicated by the parameter.  We need the `TypeFamilies` pragma for
instantiating the `MuRef` class, and `DeriveFunctor` eliminates the need to
manually instantiate a `Functor` instance for `ExprF`.

Writing `MuRef` instances is pretty easy.  For more complicated types you can
often use `Data.Traversable.traverse` in order to provide the required
implementation for `mapDeRef`
([example](https://stackoverflow.com/questions/23825800/how-to-define-a-muref-instance-for-this-nontrivial-expression-type)).

With this in place we can use `reifyGraph` from `data-reify` in order to
observe the implicit sharing.  Let's try this on a bite-sized `tree 2` and note
that it is an IO action:

    > reifyGraph (tree 2 :: Expr)
    let [(1,AddF 2 2),(2,AddF 3 3),(3,LitF 1)] in 1

Here we get an abstract syntax *graph* - rather than a tree - and sharing has
been made explicit.

We can write an interpreter for expressions by internally reifying them as
graphs and then working on those.  `reifyGraph` is an IO action, but since its
effects are pretty tame I don't feel too bad about wrapping calls to it in
`unsafePerformIO`.  Interpreting these graphs must be handled a little
differently from interpreting a tree; a naive 'tree-like' evaluator
will eliminate sharing undesirably:

``` haskell
naiveEval :: Expr -> Int
naiveEval expr = gEval reified where
  reified = unsafePerformIO $ reifyGraph expr
  gEval (Reify.Graph env r) = go r where
    go j = case lookup j env of
      Just (AddF a b) -> go a + go b
      Just (LitF d)   -> d
      Nothing         -> 0
```

This evaluator fails the `tree 50` test:

    > naiveEval (tree 50)
    -- hang

We need to use a more appropriately graph-y method to traverse and interpret
this (directed, acyclic) graph.  Here's an idea:

* [topologically sort](https://en.wikipedia.org/wiki/Topological_sorting) the
  graph, yielding a linear ordering of vertices such that for every edge
  $$u \to v$$, $$v$$ is ordered before $$u$$.
* iterate through the sorted vertices, interpreting them as desired and storing
  the interpretation
* look up the previously-interpreted vertices as needed

We can use the `Data.Graph` module from the `containers` library to deal with
the topological sorting and vertex lookups.  The following graph-based
evaluator gets the job done:

``` haskell
import Data.Graph
import Data.Maybe

graphEval :: Expr -> Int
graphEval expr = consume reified where
  reified = unsafePerformIO (toGraph <$> reifyGraph expr)
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])

consume :: Eq a => (Graph, Vertex -> (ExprF a, a, b), c) -> Int
consume (g, vmap, _) = go (reverse . topSort $ g) [] where
  go [] acc = snd $ head acc
  go (v:vs) acc =
    let nacc = evalNode (vmap v) acc : acc
    in  go vs nacc

evalNode :: Eq a => (ExprF a, b, c) -> [(a, Int)] -> (b, Int)
evalNode (LitF d, k, _)   _ = (k, d)
evalNode (AddF a b, k, _) l =
  let v = fromJust ((+) <$> lookup a l <*> lookup b l)
  in  (k, v)
```

In a serious implementation I'd want to use a more appropriate caching
structure and avoid the partial functions like `fromJust` and `head`, but you
get the point.  In any case, this evaluator passes the `tree 50` test without
issue:

    > graphEval (tree 50)
    1125899906842624

Making Sharing Explicit
-----------------------

Instead of working around the lack of sharing in our language, we can augment
it by adding the necessary sharing constructs.  In particular, we can add our
own let-binding that piggybacks on Haskell's `let`.  Here's an enhanced
language (using the same `Num` instance as before):

``` haskell
data Expr =
    Lit Int
  | Add Expr Expr
  | Let Expr (Expr -> Expr)
```

The new `Let` constructor implements *higher-order abstract syntax*, or HOAS.
There are some immediate consequences of this: we can't derive instances of our
language for typeclasses like `Eq`, `Ord`, and `Show`, and interpreting
everything becomes a bit more painful.  But, we don't need to make any use of
`data-reify` in order to share expressions, since the language now handles that
\'a la carte.  Here's an efficient evaluator:

``` haskell
eval :: Expr -> Int
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e0 e1) =
  let shared = Lit (eval e0)
  in  eval (e1 shared)
```

In particular, note that we need a sort of back-interpreter to re-embed shared
expressions into our language while interpreting them.  Here we use `Lit` to
do that, but this is more painful if we want to implement, say, a pretty
printer; in that case we need a parser such that `print (parse x) == x` (see
[here](https://www.cs.utexas.edu/~wcook/Drafts/2012/graphs.pdf)).

We also can't use the existing `tree` function.  Here's the HOAS equivalent,
which is no longer polymorphic in its return type:

``` haskell
tree :: (Num a, Eq a) => a -> Expr
tree 0 = 1
tree n = Let (tree (n - 1)) (\shared -> shared + shared)
```

Using that, we can see that sharing is preserved just fine:

    > eval (tree 50)
    1125899906842624

Another way to make sharing explicit is to use a paramterized HOAS, known as
PHOAS.  This requires the greatest augmentation of the original language
(recycling the same `Num` instance):

``` haskell
data Expr a =
    Lit Int
  | Add (Expr a) (Expr a)
  | Let (Expr a) (a -> Expr a)
  | Var a

eval :: Expr Int -> Int
eval (Lit d)     = d
eval (Var v)     = v
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e f)   = eval (f (eval e))
```

Here we parameterize the expression type and add both `Let` and `Var`
constructors to the language.  Sharing expressions explicitly now takes a
slightly different form than in the HOAS version:

``` haskell
tree :: (Num a, Eq a) => a -> Expr b
tree 0 = 1
tree n = Let (tree (n - 1)) ((\shared -> shared + shared) . Var)
```

The `Var` term wraps the intermediate computation, which is then passed to the
semantics-defining lambda.  Sharing is again preserved in the language:

    > eval $ tree 50
    1125899906842624

Here, however, we don't need the same kind of back-interpreter that we did when
using HOAS.  It's easy to write a pretty-printer that observes sharing, for
example (from [here](http://ropas.snu.ac.kr/~bruno/papers/ASGDSL.pdf)):

``` haskell
text e = go e 0 where
  go (Lit j)     _ = show j
  go (Add e0 e1) c = "(Add " ++ go e0 c ++ " " ++ go e1 c ++ ")"
  go (Var x) _     = x
  go (Let e0 e1) c = "(Let " ++ v ++ " " ++ go e0 (c + 1) ++
                     " in " ++ go (e1 v) (c + 1) ++ ")"
    where v = "v" ++ show c
```

Which yields the following string representation of our syntax:

    > putStrLn . text $ tree 2
    (Let v0 (Let v1 1 in (Add v1 v1)) in (Add v0 v0))

Cluing up
---------

I've gone over several methods of handling sharing in embedded languages:
an external memoizer, observable (implicit) sharing, and adding explicit
sharing via adding a HOAS or PHOAS let-binding to the original language.  Some
may be more convenient than others, depending on what you're trying to do.

I've dumped code for the
[minimal](https://gist.github.com/jtobin/89d741df8aaaa33eb567),
[HOAS](https://gist.github.com/jtobin/3fc26d852af9e82e378e), and
[PHOAS](https://gist.github.com/jtobin/e3e945f3c761cbc6ad43) examples in
some gists.


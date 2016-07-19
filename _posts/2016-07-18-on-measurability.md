---
layout: post
title: "On Measurability"
categories:
  - mathematics
---

.. this one is pretty dry, I'll admit.  [David Williams][williams] said it
best:

> .. Measure theory, that most arid of subjects when done for its own sake,
> becomes amazingly more alive when used in probability, not only because it is
> then applied, but also because it is immensely enriched.

What does it mean for something to be *measurable* in the mathematical sense?
Take some arbitrary collection $$X$$ and slap an appropriate algebraic
structure $$\mathcal{X}$$ on it - usually an [algebra][algebra-wiki] or
[$$\sigma$$-algebra][sa-wiki], etc.  Then we can refer to a few different
objects as 'measurable', going roughly as follows.

The elements of the structure $$\mathcal{X}$$ are called *measurable sets*.
They're called this because they can literally be assigned a notion of measure
- a kind of generalized volume.  If we're just talking about some subset of
$$X$$ out of the context of its structure then we can be pedantic and call it
measurable *with respect to* $$\mathcal{X}$$, say.  You could also call a set
$$\mathcal{X}$$-measurable, to be similarly precise.

The product of the original collection and its associated structure $$(X,
\mathcal{X})$$ is called a *measurable space*.  It's called that because it can
be completed with a measuring function $$\mu$$ - itself called a measure - that
assigns notions of measure to measurable sets.

Now take some other measurable space $$(Y, \mathcal{Y})$$ and consider a
function $$f$$ from $$X$$ to $$Y$$.  This is a *measurable function* if it
satisfies the following technical requirement: that for any
$$\mathcal{Y}$$-measurable set, its preimage under $$f$$ is an element of
$$\mathcal{X}$$ (so: the preimage under $$f$$ is $$\mathcal{X}$$-measurable).

The concept of measurability for functions probably feels the least intuitive
of the three - like one of those dry taxonomical classifications that we just
have to keep on the books.  The 'make sure your function is measurable and
everything will be ok' heuristic will get you pretty far.  But there is some
good intuition available, if you want to look for it.

Here's an example: define a set $$X$$ that consists of the elements $$A$$,
$$B$$, and $$C$$.  To talk about measurable functions, we first need to define
our measurable sets.  The de-facto default structure used for this is a
[$$\sigma$$-algebra][sa-wiki], and we can always *generate* one from some
underlying class of sets.  Let's do that from the following plain old
*partition* that splits the original collection into a couple of disjoint
'slices':

$$
H = \{\{A, B\}, \{C\}\}
$$

The $$\sigma$$-algebra $$\mathcal{X}$$ generated from this partition will just
be the partition itself, completed with the whole set $$X$$ and the empty set.
To be clear, it's the following:

$$
\mathcal{X} = \left\{\{A, B, C\}, \{A, B\}, \{C\}, \emptyset\right\}
$$

The resulting measurable space is $$(X, \mathcal{X})$$.  So we could assign a
notion of generalized volume to any element of $$\mathcal{X}$$, though I won't
actually worry about doing that here.

Now.  Let's think about some functions from $$X$$ to the real numbers, which
we'll assume to be endowed with a suitable $$\sigma$$-algebra of their own (one
typically assumes the [standard topology][st-wiki] on $$\mathbb{R}$$ and the
associated [Borel $$\sigma$$-algebra][bs-wiki]).

How about this - a simple indicator function on the slice containing $$C$$:

$$
f(x) =
  \begin{cases}
  0, \, x \in \{A, B\} \\
  1, \, x \in \{C\}
  \end{cases}
$$

Is it measurable?  That's easy to check.  The preimage of $$\{0\}$$ is $$\{A,
B\}$$, the preimage of $$\{1\}$$ is $$\{C\}$$, and the preimage of $$\{0, 1\}$$
is $$X$$ itself.  Those are all in $$\mathcal{X}$$, and the preimage of the
empty set is the empty set, so we're good.

Ok.  What about this one:

$$
g(x) =
  \begin{cases}
  0, \, x \in \{A\} \\
  1, \, x \in \{B\} \\
  2, \, x \in \{C\}
  \end{cases}
$$

Check the preimage of $$\{1, 2\}$$ and you'll find it's $$\{B, C\}$$.  But
that's *not* a member of $$\mathcal{X}$$, so $$g$$ is not measurable!

What happened here?  Failing to satisfying technical requirements aside: what,
intuitively, made $$f$$ measurable where $$g$$ wasn't?

The answer is a problem of *resolution*.  Look again at $$\mathcal{X}$$:

$$
\left\{\{A, B, C\}, \{A, B\}, \{C\}, \emptyset\right\}
$$

The structure $$\mathcal{X}$$ that we've endowed our collection $$X$$ with is
*too coarse* to permit distinguishing between elements of the slice $$\{A,
B\}$$.  There is no measurable set $$A$$, nor a measurable set $$B$$ - just
a measurable set $$\{A, B\}$$.  And as a result, if we define a function that
says something about either $$A$$ or $$B$$ without saying the same thing about
the other, *that function won't be measurable.*   The function $$f$$ assigned
the same value to both $$A$$ and $$B$$, so we didn't have any problem there.

If we want to be able to distinguish between $$A$$ and $$B$$, we'll need to
equip $$X$$ with some structure that has a finer resolution.  You can check
that if you make a measurable space out of $$X$$ and its power set (the set of
all subsets of $$X$$) then $$g$$ will be measurable there, for example.

So if we're using partitions to define our measurable sets, we get a neat
little property: for any measurable function, elements in the same slice of the
partition *must* have the same value when passed through the function.  So if
you have a function $$h : X \to H$$ that takes an element to its respective
slice in a partition, you know that $$h(x_{0}) = h(x_{1})$$ for any $$x_{0}$$,
$$x_{1}$$ in $$X$$ implies that $$f(x_{0}) = f(x_{1})$$ for any measurable
function $$f$$.

## Addendum

Whipping together a measurable space using a $$\sigma$$-algebra generated by a
partition of sets occurs naturally when we talk about [correlated
equilibrium][ce-wiki], a solution concept in non-cooperative game theory.  It's
common to say a function - in that context a *correlated strategy* - must be
measurable 'with respect to the partition', which sort of elides the fact that
we still need to generate a $$\sigma$$-algebra from it anyway.

Some oldschool authors (Halmos, at least) developed their measure theory using
[$$\sigma$$-rings][ring-wiki], but this doesn't seem very popular nowadays.
Since a ring doesn't require including the entire set $$X$$, you need to go
through an awkward extra hoop when defining measurability on functions.  But
regardless, it's interesting to think about what happens when one uses
different structures to define measurable sets!

[williams]: https://www.amazon.com/Probability-Martingales-Cambridge-Mathematical-Textbooks/dp/0521406056
[ring-wiki]: https://en.wikipedia.org/wiki/Ring_of_sets
[algebra-wiki]: https://en.wikipedia.org/wiki/Algebra_of_sets
[ce-wiki]: https://en.wikipedia.org/wiki/Correlated_equilibrium
[sa-wiki]: https://en.wikipedia.org/wiki/Sigma-algebra
[st-wiki]: https://en.wikipedia.org/wiki/Topological_space#Examples_of_topological_spaces
[bs-wiki]: https://en.wikipedia.org/wiki/Borel_set

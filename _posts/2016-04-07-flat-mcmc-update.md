---
layout: post
title: "flat-mcmc Update and v1.0.0 Release"
categories:
  - haskell
  - statistics
---

I've updated my old [*flat-mcmc*](https://github.com/jtobin/flat-mcmc) library
for ensemble sampling in Haskell and have pushed out a v1.0.0 release.

## History

I wrote *flat-mcmc* in 2012, and it was the first serious-ish size project I
attempted in Haskell.  It's an implementation of Goodman & Weare's [affine
invariant ensemble
sampler](http://msp.org/camcos/2010/5-1/camcos-v5-n1-p04-p.pdf), a Monte Carlo
algorithm that works by running a Markov chain over an ensemble of particles.
It's easy to get started with (there are no tuning parameters, etc.) and
is sufficiently robust for a lot of purposes.  The algorithm became somewhat
famous in the astrostatistics community, where some of its members implemented
it via the very nice and polished Python library,
[emcee](http://dan.iel.fm/emcee/current/).

The library has become my second-most starred repo on Github, with a whopping
10 stars as of this writing (the Haskell MCMC community is pretty niche, bro).
Lately someone emailed me and asked if I wouldn't mind pushing it to Stackage,
so I figured it was due for an update and gave it a little modernizing along
the way.

I'm currently on sabbatical and am traveling through Vietnam; I started the
rewrite in Hanoi and finished it in Saigon, so it was a kind of nice side
project to do while sipping coffees and the like during downtime.

## What Is It

I wrote a little summary of the library in 2012, which you can still find
[tucked away on my personal site](http://jtobin.ca/flat-mcmc/).  Check that out
if you'd like a description of the algorithm and why you might want to use it.

Since I wrote the initial version my astrostatistics-inclined friends David
Huijser and Brendon Brewer [wrote a paper](http://arxiv.org/abs/1509.02230)
about some limitations they discovered when using this algorithm in
high-dimensional settings.  So caveat emptor, buyer beware and all that.

In general this is an extremely easy-to-use algorithm that will probably get
you decent samples from arbitrary targets without tedious tuning/fiddling.

## What's New

I've updated and standardized the API in line with my other MCMC projects
huddled around the [declarative](http://jtobin.ca/markov-chains-a-la-carte)
library.  That means that, like the others, there are two primary ways to use
the library: via an `mcmc` function that will print a trace to stdout, or a
`flat` transition operator that can be used to work with chains in memory.

Regrettably you can't use the `flat` transition operator with others in the
`declarative` ecosystem as it operates over *ensembles*, whereas the others are
single-particle algorithms.

The README over at the [Github repo](https://github.com/jtobin/flat-mcmc)
contains a brief usage example.  If there's some feature you'd like to see or
documentation/examples you could stand to have added then don't hestitate to
ping me and I'll be happy to whip something up.

In the meantime I've pushed a new version to
[Hackage](https://hackage.haskell.org/package/flat-mcmc) and added the library
to [Stackage](https://www.stackage.org/), so it should show up in an LTS
release soon enough.

Cheers!


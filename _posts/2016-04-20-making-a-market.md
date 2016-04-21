---
layout: post
title: "Making a Market"
categories:
  - probability
  - statistics
---

Suppose you're in the derivatives business.  You are interested in making a
market on some events; say, whether or not your friend Jay will win tomorrow
night's poker game, or that the winning pot will be at least USD 100.  Let's
examine some rules about how you should do business if you want this venture to
succeed.

What do I mean by 'make a market'?  I mean that you will be willing to buy and
sell units of a particular security that will be redeemable from the seller at
some particular value after tomorrow's poker game has ended (you will be making
a simple *prediction market*, in other words).  You can make bid offers to buy
securities at some price, and ask offers to sell securities at some price.

To keep things simple let's say you're doing this gratis; society rewards you
extrinsically for facilitating the market - your friends will give you free
pizza at the game, maybe - so you won't levy any *transaction fees* for making
trades.  Also scarcity isn't a huge issue, so you're willing to buy or sell any
finite number of securities.

Consider the possible outcomes of the game (one and only one of which must
occur):

1. (A) Jay wins and the pot is at least USD 100.
2. (B) Jay wins and the pot is less than USD 100.
3. (C) Jay loses and the pot is at least USD 100.
4. (D) Jay loses and the pot is less than USD 100.

The securities you are making a market on pay USD 1 if an event occurs, and USD
0 otherwise.  So: if I buy 5 securities on outcome $$A$$ from you, and outcome
$$A$$ occurs, I'll be able to go to you and redeem my securities for a total of
USD 5.  Alternatively, if I sell you 5 securities on outcome $$A$$, and outcome
$$A$$ occurs, you'll be able to come to me and redeem your securities for a
total of USD 5.

Consider what that implies: as a market maker, you face the prospect of making
hefty payments to customers who redeem valuable securities.  For example,
imagine the situation where you charge USD 0.50 for a security on outcome
$$A$$, but outcome $$A$$ is almost certain to occur in some sense (Jay is a
beast when it comes to poker and a lot of high rollers are playing); if your
customers exclusively load up on 100 cheap securities on outcome $$A$$, and
outcome $$A$$ occurs, then you stand to owe them a total payment of USD 100
against the USD 50 that they have paid for the securities.  You thus have a
heavy incentive to price your securities as accurately as possible - where
'accurate' means to minimize your expected loss.

It may always be the case, however, that it is difficult to price your
securities accurately.  For example, if some customer has more information than
you (say, she privately knows that Jay is unusually bad at poker) then she
potentially stands to profit from holding said information in lieu of your
ignorance on the matter (and that of your prices).   Such is life for a market
maker.  But there are particular prices you could offer - independent of any
participant's private information - that are plainly stupid or ruinous for you
(a set of prices like this is sometimes called a [Dutch
book](https://en.wikipedia.org/wiki/Dutch_book)).  Consider selling securities
on outcome $$A$$ for the price of USD -1; then anyone who buys one of these
securities not only stands to redeem USD 1 in the event outcome $$A$$ occurs,
but also gains USD 1 simply from the act of buying the security in the first
place.

Setting a negative price like this is irrational on your part; customers will
realize an *arbitrage opportunity* on securities for outcome $$A$$ and will
happily buy as many as they can get their hands on, to your ruin.  In other
words - and to nobody's surprise - by setting a negative price, **you can be
made a sure loser** in the market.

There are other prices you should avoid setting as well, if you want to avoid
arbitrage opportunities like these.  For starters:

* For any outcome $$E$$, you must set the price of a security on $$E$$ to be at
  least USD 0.
* For any *certain* outcome $$E$$, you must set the price of a security on $$E$$ to
  be exactly USD 1.

The first condition rules out negative prices, and the second ensures that your
books balance when it comes time to settle payment for securities on a certain
event.

What's more, the price that you set on any given security doesn't exist in
isolation.  Given the outcomes $$A$$, $$B$$, $$C$$, and $$D$$ listed previously, at
least one *must* occur.   So as per the second rule, the price of a synthetic
derivative on the outcome "Jay wins or loses, and the pot is any value" must be
set to USD 1.  This places constraints on the prices that you can set for
individual securities.  It suffices that:

* For any countable set of mutually exclusive outcomes $$E_{1}, E_{2}, \ldots$$,
  you must set the price of the security on outcome "$$E_{1}$$ or $$E_{2}$$ or.."
  to exactly the sum of the prices of the individual outcomes.

This eliminates the possibility that your customers will make you a certain
loser by buying elaborate combinations of securities on different outcomes.

There are other rules that your prices must obey as well, but they fall out as
corollaries of these three.  If you broke any of themm you'd also be breaking
one of these.

It turns out that you *cannot be made a sure loser if, and only if, your prices
obey these three rules*.  That is:

* If your prices follow these rules, then you will offer customers no arbitrage
  opportunities.
* Any market absent of arbitrage opportunities must have prices that conform
  to these rules.

These prices are called *coherent*, and absence of coherence implies the
existence of arbitrage opportunities for your customers.

## But Why Male Models

The trick, of course, is that these prices correspond to *probabilities*, and
the rules for avoiding arbitrage correspond to the standard [Kolmogorov
axioms](https://en.wikipedia.org/wiki/Probability_axioms) of probability
theory.

The consequence is that if your description of uncertain phenomena does not
involve probability theory, or does not behave exactly like probability theory,
then it is an *incoherent* representation of information you have about those
phenomena.

As a result, probability theory should be your tool of choice when it comes
to describing uncertain phenomena.  Granted you may not have to worry about
market making in return for pizza, but you'd like to be assured that there are
no structural problems with your description.

## Comments

This is a summary of the development of probability presented in Jay Kadane's
brilliant [Principles of Uncertainty](http://uncertainty.stat.cmu.edu/).  The
original argument was developed by de Finetti and Savage in the mid-20th
century.

Kadane's book makes for an exceptional read, and it's free to boot.  I
recommend checking it out if it has flown under your radar.

An interesting characteristic of this development of probability is that there
is no way to guarantee the nonexistence of arbitrage opportunities for a
countably infinite number of purchased securities.  That is: if you're a market
maker, you could be made a sure loser in the market when it came time for you
to settle a countably infinite number of redemption claims.  The quirk here is
that you could also be made a sure winner as well; whether you win or lose with
certainty depends on the order in which the claims are settled!  (Fortunately
this doesn't tend to be an issue in practice.)

Thanks to [Fredrik Olsen](http://medium.com/@folsen) for reviewing a draft of
this post.

## References

* [Principles of Uncertainty](http://uncertainty.stat.cmu.edu/)
* [On De Finetti coherence and Kolmogorov probability](http://www.mit.edu/~mitter/publications/102_ondefinetti_elsev.pdf)
* [Lost Causes in Statistics I: Finite Additivity](https://normaldeviate.wordpress.com/2013/06/30/lost-causes-in-statistics-i-finite-additivity/)
* [De Finetti, Countable Additivity, Consistency and Coherence](http://joelvelasco.net/teaching/3865/howson%20-%20de%20finetti%20countable%20additivity.pdf)
* [Finite Additivity Versus Countable Additivity: De Finetti and Savage](http://wwwf.imperial.ac.uk/~bin06/Papers/favcarev.pdf)


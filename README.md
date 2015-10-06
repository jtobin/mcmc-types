# mcmc-types [![Build Status](https://secure.travis-ci.org/jtobin/mcmc-types.png)](http://travis-ci.org/jtobin/mcmc-types)

Markov Chain Monte Carlo (MCMC) algorithms are useful for approximating
integrals in Bayesian statistics.

Typically one wants to integrate (something proportional to) a probability
density over some state space corresponding to a model's parameters.  Coming up
with a suitable grid of points when approximating an integral can be hard, but
using a Markov chain to do the dirty work offloads the problem to probability
theory.

The idea is that the Markov chain wanders around the state space such that, in
the limit, it visits regions of the space in proportion to their probability.
The points that it visits can then be used to approximate integrals.  This is
usually preferable to stateless Monte Carlo methods like rejection or
importance sampling when the number of dimensions is high.

An instance of an MCMC problem can be characterized by the following types:

* A *target distribution* over some parameter space
* A *parameter space* for the chain to wander over
* A *transition operator* to drive the Markov chain

The *mcmc-types* library provides generic definitions for each of these.

`Target` is a product type intended to hold a log-target density function and
potentially its gradient.

The `Chain` type represents a kind of 'annotated' parameter space.  Technically
all that's required is the type of the parameter space itself (held here in
`chainPosition`) but in practice some additional information is typically
useful.

The `Transition` type permits probabilistic transitions over some state space
by way of the underlying `Prob` monad.

See e.g. the [mighty-metropolis](http://github.com/jtobin/mighty-metropolis)
library for example use.


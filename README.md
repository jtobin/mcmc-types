# mcmc-types [![Build Status](https://secure.travis-ci.org/jtobin/mcmc-types.png)](http://travis-ci.org/jtobin/mcmc-types)

Common types for implementing Markov Chain Monte Carlo (MCMC) algorithms.

An instance of an MCMC problem can be characterized by the following:

* A *target distribution* over some parameter space
* A *parameter space* for a Markov chain to wander over
* A *transition operator* to drive the Markov chain

*mcmc-types* provides the suitably-general `Target`, `Chain`, and
`Transition` types for usefully representing these things respectively.

* `Target` is a product type intended to hold a log-target density function and
  potentially its gradient.

* The `Chain` type represents an 'annotated' parameter space.  Technically all
  that's required is the type of the parameter space itself (held here in
  `chainPosition`) but in practice some additional information is typically
  useful.  The `chainTunables` field can be used to hold arbitrary data; one
  should avoid using it to do something nasty like, say, invalidating the Markov
  property.

* The `Transition` type permits probabilistic transitions over some state space
  by way of the underlying `Prob` monad.

See e.g. the [mighty-metropolis](http://github.com/jtobin/mighty-metropolis)
library for example use.


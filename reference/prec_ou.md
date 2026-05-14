# Generator function for an Ornstein-Uhlenbeck process

Internal function that calculates the precision matrix for a 1d 1st
order continuous-time autoregressive random walk (I.e. an
Ornstein-Uhlenbeck, or OU process). Takes two vectors of indices (start
and end) saying which nodes out of the n total nodes are connected to
each other. The `alpha` parameter controls the strength of the
autoregressive component relative to the ends of function.

## Usage

``` r
prec_ou(start, end, n, dists, alpha)
```

## Arguments

- start:

  vector of starting indices. All values must be between 1 and n

- end:

  vector of ending indices (nodes that follow the start node in the
  list)

- n:

  Total number of nodes in the system

- dists:

  The distances between the pairs of nodes. Must be positive numbers.

- alpha:

  The autocorrelation strength of the random walk. Must range between
  1e-5 and infinity, with the lower limit set to avoid numerical issues
  with dividing by very small numbers

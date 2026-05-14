# Generator function for a 1st order discrete-time autoregessive process.

Internal function that calculates the precision matrix for a 1d 1st
order discrete-time autoregressive random walk. Takes two vectors of
indices (start and end) saying which nodes out of the n total nodes are
connected to each other. The `rho` parameter controls the strength of
the autoregressive component relative to the ends of function.

## Usage

``` r
prec_ar1(start, end, n, dists, rho)
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

  The distances between the pairs of nodes. Must be positive integers.

- rho:

  The autocorrelation strength of the random walk. Must range between -1
  and 1.

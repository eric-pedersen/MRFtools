# Generator for 1st order randm walk

Generator for 1st order randm walk

Internal function for generating precision matrices for 1st order RWs
for 1D data. This is the same as a Brownian motion stochastic process
sampled at a finite number of points

## Usage

``` r
prec_rw1(start, end, n, dists)
```

## Arguments

- start:

  Indices for the starting value of an arrow connecting two obs

- end:

  Indices for the starting value of an arrow connecting two obs. Must be
  the same length as `start`.

- n:

  Total number of values for the smoother

- dists:

  Distances between pairs of observations. Must be a vector of positive
  values of the same length as `start`

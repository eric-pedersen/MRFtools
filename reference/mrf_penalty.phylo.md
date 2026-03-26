# MRF penalty from a phylogeny

MRF penalty from a phylogeny

## Usage

``` r
# S3 method for class 'phylo'
mrf_penalty(
  object,
  model = c("rw1", "Brownian"),
  node_labels = NULL,
  delta = FALSE,
  eps = 0,
  ...
)
```

## Arguments

- object:

  an R object to create the MRF penalty from.

- model:

  character; one of `"full"` or `"individual"` indicating if a fully
  connected graph (`"full"`) or a random effect (random intercepts;
  `"individual"`) penalty is created.

- node_labels:

  character; a vector of alternative labels for the levels of the
  factor.

- delta:

  numeric or logical; either the numeric value to add to the diagonal of
  the MRF penalty matrix, or a logical value indicating if such an
  adjustment should be made. The default is to not alter the diagonal of
  the penalty matrix.

- eps:

  A value to add to the variance-covariance matrix diagonal to make it
  positive definite

- ...:

  arguments passed to other methods.

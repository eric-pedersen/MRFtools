# MRF penalty from a dendrogram

MRF penalty from a dendrogram

## Usage

``` r
# S3 method for class 'dendrogram'
mrf_penalty(
  object,
  model = c("rw1", "ou", "brownian"),
  alpha = NULL,
  at_tips = NULL,
  internal_nodes = TRUE,
  delta = FALSE,
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

- alpha:

  numeric; the autoregressive parameter for an OU stochastic process.
  Should be \>1e-5 (alpha = 0 would correspond to a "rw1" model).

- at_tips:

  character; vector of tip labels to calculate the penalty at. All
  values in the vector must correpond to tip name labels in the tree.

- internal_nodes:

  logical; should the internal nodes of the tree be included in the
  penalty (`TRUE`), or just the tips (terminal nodes; `FALSE`)

- delta:

  numeric or logical; either the numeric value to add to the diagonal of
  the MRF penalty matrix, or a logical value indicating if such an
  adjustment should be made. The default is to not alter the diagonal of
  the penalty matrix.

- ...:

  arguments passed to other methods.

# MRF penalty from a dendrogram

MRF penalty from a dendrogram

## Usage

``` r
# S3 method for class 'dendrogram'
mrf_penalty(object, model = NULL, node_labels = NULL, delta = FALSE, ...)
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

- ...:

  arguments passed to other methods.

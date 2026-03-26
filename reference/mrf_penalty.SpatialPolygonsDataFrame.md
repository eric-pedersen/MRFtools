# MRF penalty from a SpatialPoylgonsDataFrame

MRF penalty from a SpatialPoylgonsDataFrame

## Usage

``` r
# S3 method for class 'SpatialPolygonsDataFrame'
mrf_penalty(
  object,
  model = "icar",
  node_labels = NULL,
  buffer = NULL,
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

- node_labels:

  character; a vector of alternative labels for the levels of the
  factor.

- buffer:

  numeric; buffer distance for all or for individual elements of the
  geometry. See argument `dist` in
  [sf::st_buffer](https://r-spatial.github.io/sf/reference/geos_unary.html)
  for details.

- delta:

  numeric or logical; either the numeric value to add to the diagonal of
  the MRF penalty matrix, or a logical value indicating if such an
  adjustment should be made. The default is to not alter the diagonal of
  the penalty matrix.

- ...:

  arguments passed to other methods.

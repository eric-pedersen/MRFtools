# Continuous-time random walk MRF penalty from a numeric vector

Models one-dimensional numeric vectors as random-walk models.

## Usage

``` r
# S3 method for class 'numeric'
mrf_penalty(
  object,
  model = c("rw1", "rw2", "ar1", "ou"),
  cyclic = FALSE,
  rho = NULL,
  at_nodes = NULL,
  node_labels = NULL,
  add_missing = NULL,
  end_points = NULL,
  end_dist = NULL,
  delta = FALSE,
  ...,
  type
)
```

## Arguments

- object:

  an R object to create the MRF penalty from.

- model:

  character; one of "rw1", "rw2", "ar1", or "ou". See Description for
  details on the models

- cyclic:

  logical; If TRUE, the end points are treated as neighbouring each
  other. See Description for details

- rho:

  numeric;

- at_nodes:

  numeric;

- node_labels:

  character; a vector of alternative labels for the levels of the
  factor.

- add_missing:

  logical;

- end_points:

  numeric; an optional vector of length 2 providing the end points of
  the period of cycle.

- end_dist:

  numeric;

- delta:

  numeric or logical; either the numeric value to add to the diagonal of
  the MRF penalty matrix, or a logical value indicating if such an
  adjustment should be made. The default is to not alter the diagonal of
  the penalty matrix.

- ...:

  arguments passed to other methods.

- type:

  character; deprecated. Use, `model` instead.

## Examples

``` r
# linear rw1: 1st order continuous-time random walk
p <- mrf_penalty(1:10)
as.matrix(p)
#>     1  2  3  4  5  6  7  8  9 10
#> 1   1 -1  0  0  0  0  0  0  0  0
#> 2  -1  2 -1  0  0  0  0  0  0  0
#> 3   0 -1  2 -1  0  0  0  0  0  0
#> 4   0  0 -1  2 -1  0  0  0  0  0
#> 5   0  0  0 -1  2 -1  0  0  0  0
#> 6   0  0  0  0 -1  2 -1  0  0  0
#> 7   0  0  0  0  0 -1  2 -1  0  0
#> 8   0  0  0  0  0  0 -1  2 -1  0
#> 9   0  0  0  0  0  0  0 -1  2 -1
#> 10  0  0  0  0  0  0  0  0 -1  1

# cyclic rw1:
p <- mrf_penalty(1:10, model = "rw1", cyclic = TRUE)
as.matrix(p)
#>     1  2  3  4  5  6  7  8  9 10
#> 1   2 -1  0  0  0  0  0  0  0 -1
#> 2  -1  2 -1  0  0  0  0  0  0  0
#> 3   0 -1  2 -1  0  0  0  0  0  0
#> 4   0  0 -1  2 -1  0  0  0  0  0
#> 5   0  0  0 -1  2 -1  0  0  0  0
#> 6   0  0  0  0 -1  2 -1  0  0  0
#> 7   0  0  0  0  0 -1  2 -1  0  0
#> 8   0  0  0  0  0  0 -1  2 -1  0
#> 9   0  0  0  0  0  0  0 -1  2 -1
#> 10 -1  0  0  0  0  0  0  0 -1  2

# cyclic with user-specified end points
p <- mrf_penalty(1:10, model = "rw1", cyclic = TRUE, end_points = c(0,11))
as.matrix(p)
#>             1  2  3  4  5  6  7  8  9         10
#> 1   1.3333333 -1  0  0  0  0  0  0  0 -0.3333333
#> 2  -1.0000000  2 -1  0  0  0  0  0  0  0.0000000
#> 3   0.0000000 -1  2 -1  0  0  0  0  0  0.0000000
#> 4   0.0000000  0 -1  2 -1  0  0  0  0  0.0000000
#> 5   0.0000000  0  0 -1  2 -1  0  0  0  0.0000000
#> 6   0.0000000  0  0  0 -1  2 -1  0  0  0.0000000
#> 7   0.0000000  0  0  0  0 -1  2 -1  0  0.0000000
#> 8   0.0000000  0  0  0  0  0 -1  2 -1  0.0000000
#> 9   0.0000000  0  0  0  0  0  0 -1  2 -1.0000000
#> 10 -0.3333333  0  0  0  0  0  0  0 -1  1.3333333
```

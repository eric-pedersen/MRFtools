# Continuous-time random walk MRF penalty from a numeric vector

Models one-dimensional numeric vectors as random-walk models.

## Usage

``` r
# S3 method for class 'numeric'
mrf_penalty(
  object,
  model = c("rw1", "rw2", "rw2_d", "ar1", "ou"),
  cyclic = FALSE,
  alpha = NULL,
  rho = NULL,
  at_nodes = NULL,
  node_labels = NULL,
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

  character; one of "rw1","ou", "ar1", "rw2", or "rw2_d". "rw1" is a
  first-order continuous-time random walk model (I.e. Brownian motion).
  "ou" is the Ornstein-Uhlenbeck (I.e. continuous-time first-order
  autoregressive model); model = "ou" also requires specifying a value
  of alpha. "ar1" is a first-order discrete-time autoregressive model,
  and requires specifying the `rho` parameter. "rw2" is a second-order
  random-walk model (the "CRW2" model from Rue and Held 2005); it
  returns a dense precision matrix for the values of the function
  evaluated at the chosen points. "rw2_d" is a sparse version of "rw2",
  that includes both the function itself and the derivatives of the
  function in its precision matrix. See Description for details on the
  models

- cyclic:

  logical; If TRUE, the end points are treated as neighbouring each
  other. See Description for details

- alpha:

  numeric; autoregression parameter for a continuous-time random walk
  ("ou"). rho must be \>0.

- rho:

  numeric; autoregression parameter for a discrete-time random walk
  ("ar1"). abs(rho) must be \< 1 if specified.

- at_nodes:

  numeric; what nodes (I.e. object values) that you want to evaluate the
  penalty at. Must include all values levels specified in `object`, but
  can also include additional levels that you want to evaluate the
  penalty at (see details).

- node_labels:

  character; a vector of alternative labels for the levels of the
  factor.

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
# create some test data
x_cont <- seq(0,5, length = 10)
x_disc <- 1:10 

# linear rw1: 1st order continuous-time random walk
p1 <- mrf_penalty(x_cont)
p1
#> Markov Random Field penalty
#> Type: sequential
#> N   : 10

# cyclic rw1:
p2 <- mrf_penalty(x_cont, model = "rw1", cyclic = TRUE)
p2
#> Markov Random Field penalty
#> Type: cyclic
#> Type: sequential
#> N   : 10

# cyclic with user-specified end points
p3 <- mrf_penalty(x_cont, model = "rw1", cyclic = TRUE, end_points = c(0,6))
p3
#> Markov Random Field penalty
#> Type: cyclic
#> Type: sequential
#> N   : 10

# Continuous-time auto-regressive (I.e. "ou") model:
p4 <- mrf_penalty(x_cont, model = "ou",alpha = 2)
p4
#> Markov Random Field penalty
#> Type: sequential
#> N   : 10

# Discrete-time autoregressive model with negative 1st order autocorrelation
p5 <- mrf_penalty(x_disc, model = "ar1", rho = -0.5)
```

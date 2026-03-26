# Fully connected graph and random effect MRF penalties from a factor

Fully connected graph and random effect MRF penalties from a factor

## Usage

``` r
# S3 method for class 'factor'
mrf_penalty(
  object,
  model = c("full", "individual"),
  node_labels = NULL,
  delta = FALSE,
  ...,
  type
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

- ...:

  arguments passed to other methods.

- type:

  character; deprecated. Use, `model` instead.

## Examples

``` r
# a factor
fv <- factor(letters[1:10])

# create the MRF penalty for a fully connected graph
p <- mrf_penalty(fv, model = "full")
p
#> Markov Random Field penalty
#> Type: categorical
#> N   : 10
as.matrix(p)
#>    a  b  c  d  e  f  g  h  i  j
#> a  9 -1 -1 -1 -1 -1 -1 -1 -1 -1
#> b -1  9 -1 -1 -1 -1 -1 -1 -1 -1
#> c -1 -1  9 -1 -1 -1 -1 -1 -1 -1
#> d -1 -1 -1  9 -1 -1 -1 -1 -1 -1
#> e -1 -1 -1 -1  9 -1 -1 -1 -1 -1
#> f -1 -1 -1 -1 -1  9 -1 -1 -1 -1
#> g -1 -1 -1 -1 -1 -1  9 -1 -1 -1
#> h -1 -1 -1 -1 -1 -1 -1  9 -1 -1
#> i -1 -1 -1 -1 -1 -1 -1 -1  9 -1
#> j -1 -1 -1 -1 -1 -1 -1 -1 -1  9

# create the MRF penalty equivalent of random effects
p <- mrf_penalty(fv, model = "individual")
p
#> Markov Random Field penalty
#> Type: categorical
#> N   : 10
as.matrix(p)
#>   a b c d e f g h i j
#> a 1 0 0 0 0 0 0 0 0 0
#> b 0 1 0 0 0 0 0 0 0 0
#> c 0 0 1 0 0 0 0 0 0 0
#> d 0 0 0 1 0 0 0 0 0 0
#> e 0 0 0 0 1 0 0 0 0 0
#> f 0 0 0 0 0 1 0 0 0 0
#> g 0 0 0 0 0 0 1 0 0 0
#> h 0 0 0 0 0 0 0 1 0 0
#> i 0 0 0 0 0 0 0 0 1 0
#> j 0 0 0 0 0 0 0 0 0 1
```

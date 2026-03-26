# Extract a fitted MRF

Extract a fitted MRF

## Usage

``` r
get_mrf(object, ...)

# S3 method for class 'bam'
get_mrf(object, ...)

# S3 method for class 'gamm'
get_mrf(object, ...)

# S3 method for class 'list'
get_mrf(object, ...)

# S3 method for class 'gam'
get_mrf(object, term, ...)
```

## Arguments

- object:

  An object from which to extract the fitted MRF. Currently only for
  objects of classes `gam`, `bam`, and `gamm`, and GAMMs fitted by
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html).

- ...:

  Arguments passed to other methods.

- term:

  character; the MRF term to extract. Can be a partial match to a term,
  which is matched against the smooth label.

## Value

A object representing the fitted MRF

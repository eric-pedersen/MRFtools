# Extract the original object used to construct an MRF penalty

Extract the original object used to construct an MRF penalty

## Usage

``` r
get_obj(object)

# S3 method for class 'mrf_config'
get_obj(object)

# S3 method for class 'mrf_penalty'
get_obj(object)
```

## Arguments

- object:

  an object of class `"mrf_penalty"` or `"mrf_config"`.

## Value

A length 1 character vector containing the type of MRF penalty.

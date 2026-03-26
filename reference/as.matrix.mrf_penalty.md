# Convert a MRF penalty object to a matrix

Convert a MRF penalty object to a matrix

## Usage

``` r
# S3 method for class 'mrf_penalty'
as.matrix(x, ...)
```

## Arguments

- x:

  an object inheriting from class `"mrf_penalty"`

- ...:

  arguments passed to other methods

## Examples

``` r
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
```

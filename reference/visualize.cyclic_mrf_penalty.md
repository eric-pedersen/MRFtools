# Visualizing penalty matrix or graph object for a cyclic 1D MRF

Visualizing penalty matrix or graph object for a cyclic 1D MRF

## Usage

``` r
# S3 method for class 'cyclic_mrf_penalty'
visualize(x, graph = TRUE, layout = "linear", circular = TRUE, ...)
```

## Arguments

- x:

  an object of class `"cyclic_mrf_penalty"`

- graph:

  logical;

- layout:

  character;

- circular:

  logical;

- ...:

  arguments passed to other methods and ultimately on to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  if `graph = TRUE`.

## Examples

``` r
# example code
mrf_penalty(1:10, type = "linear") |>
  visualize()
```

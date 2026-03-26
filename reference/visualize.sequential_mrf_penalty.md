# Plot a thing

Plot a thing

## Usage

``` r
# S3 method for class 'sequential_mrf_penalty'
visualize(
  x,
  graph = TRUE,
  layout = "linear",
  circular = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_scale = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `"sequential_mrf_penalty"`

- graph:

  logical;

- layout:

  character;

- circular:

  logical;

- xlab, ylab, title, subtitle, caption:

  character; labels for plots. If `xlab` or `ylab` are not supplied, a
  suitable default is used.

- fill_scale:

  a suitable fill scale to use if plotting the penalty matrix

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

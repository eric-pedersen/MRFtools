# test the numeric method
test_that("linear mrf_penalty with a numeric", {
  expect_silent(pen <- mrf_penalty(nv))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the numeric method with cyclic option
test_that("cyclic mrf_penalty with a numeric", {
  expect_silent(pen <- mrf_penalty(nv, cyclic=TRUE))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the numeric method with cyclic option and endpoint
test_that("cyclic mrf_penalty with a numeric and user end points", {
  expect_silent(pen <- mrf_penalty(nv, cyclic = TRUE, end_points = c(0, 11)))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the factor method for fully connected graph
test_that("fully connected mrf_penalty with a factor", {
  expect_silent(pen <- mrf_penalty(fv, model = "full"))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the factor method for individual aka ranef
test_that("ranef mrf_penalty with a factor", {
  expect_silent(pen <- mrf_penalty(fv, model = "individual"))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the dendrogram method
test_that("mrf_penalty with a dendrogram", {
  expect_silent(pen <- mrf_penalty(as.dendrogram(hc)))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the hclust method
test_that("mrf_penalty with a hclust", {
  expect_silent(pen <- mrf_penalty(hc))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
})

# test the phylo4 method
test_that("mrf_penalty with a phylo4 ojbect", {
  expect_silent(pen <- mrf_penalty(tr4))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_equal(dim(pen), c(7,7))
  expect_s3_class(pen, "mrf_penalty")
  
  #testing reduced matrix function
  expect_silent(pen2 <- mrf_penalty(tr4,at_tips = c("sp1","sp2","sp4")))
  expect_snapshot(print(as.matrix(pen2)), variant = "matrix")
  expect_equal(dim(pen2), c(5,5))
  
  #testing tip-only matrix
  expect_silent(pen3 <- mrf_penalty(tr4,internal_nodes = FALSE))
  expect_snapshot(print(as.matrix(pen3)), variant = "matrix")
  expect_equal(dim(pen3), c(4,4))
  
  #testing the OU model
  expect_error(
    mrf_penalty(tr4,model = "ou"),
    regexp = "one and only one of 'alpha' or 'rho'"
    )
  expect_silent(pen4 <- mrf_penalty(tr4,model = "ou", params = list(rho = 0.5)))
  expect_snapshot(print(as.matrix(pen4)), variant = "matrix")
  
})

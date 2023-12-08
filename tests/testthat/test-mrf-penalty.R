# test the numeric method
test_that("linear mrf_penalty with a numeric", {
    expect_silent(pen <- mrf_penalty(nv))
    expect_snapshot(print(pen), variant = "print")
    expect_snapshot(print(as.matrix(pen)), variant = "matrix")
    expect_s3_class(pen, "mrf_penalty")
})

# test the numeric method with cyclic option
test_that("cyclic mrf_penalty with a numeric", {
    expect_silent(pen <- mrf_penalty(nv, type = "cyclic"))
    expect_snapshot(print(pen), variant = "print")
    expect_snapshot(print(as.matrix(pen)), variant = "matrix")
    expect_s3_class(pen, "mrf_penalty")
})

# test the numeric method with cyclic option and endpoint
test_that("cyclic mrf_penalty with a numeric and user end points", {
    expect_silent(pen <- mrf_penalty(nv, type = "cyclic",
        end_points = c(0, 11)))
    expect_snapshot(print(pen), variant = "print")
    expect_snapshot(print(as.matrix(pen)), variant = "matrix")
    expect_s3_class(pen, "mrf_penalty")
})

# test the factor method for fully connected graph
test_that("fully connected mrf_penalty with a factor", {
    expect_silent(pen <- mrf_penalty(fv, type = "full"))
    expect_snapshot(print(pen), variant = "print")
    expect_snapshot(print(as.matrix(pen)), variant = "matrix")
    expect_s3_class(pen, "mrf_penalty")
})

# test the factor method for individual aka ranef
test_that("ranef mrf_penalty with a factor", {
    expect_silent(pen <- mrf_penalty(fv, type = "individual"))
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

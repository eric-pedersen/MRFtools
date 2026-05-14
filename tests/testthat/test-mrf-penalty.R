#### testing sequential methods 
test_that("linear sequential mrf_penalties", {
  expect_silent(seq_pen1 <- mrf_penalty(nv))
  expect_snapshot(print(seq_pen1), variant = "print")
  expect_snapshot(print(as.matrix(seq_pen1)), variant = "matrix")
  expect_s3_class(seq_pen1, "mrf_penalty")
  expect_s3_class(seq_pen1, "matrix")
  expect_silent(seq_pen2 <- mrf_penalty(nv/2))
  expect_equal(
    diag(seq_pen2,names = FALSE), 
    rep(c(1,2,1), times = c(1,n-2,1))
    )
  expect_equal(
    diag(seq_pen2[-1,-n],names = FALSE), 
    rep(-1, times = n-1)
    )
  expect_equal(
    as.numeric(colSums(seq_pen2)), 
    rep(0,times= n)
    )
})

#### test sequential methods with cyclic option ###
test_that("cyclic sequential mrf_penalty", {
  expect_silent(pen_cyclic <- mrf_penalty(nv, cyclic=TRUE))
  expect_snapshot(print(pen_cyclic), variant = "print")
  expect_snapshot(print(as.matrix(pen_cyclic)), variant = "matrix")
  expect_s3_class(pen_cyclic, "mrf_penalty")
})

# test the numeric method with cyclic option and endpoint
test_that("cyclic sequential mrf_penalty with user-specified end points", {
  expect_silent(pen <- mrf_penalty(nv, cyclic = TRUE, end_points = c(0, 11)))
  expect_snapshot(print(pen), variant = "print")
  expect_snapshot(print(as.matrix(pen)), variant = "matrix")
  expect_s3_class(pen, "mrf_penalty")
  expect_equal(get_model(pen)$parameters$end_points, c(0,11))
  expect_equal(get_model(pen)$parameters$end_dist, 1)
})

#test the CW2 methods
test_that("linear sequential rw2 mrf_penalty",{
  expect_silent(pen_cw2 <- mrf_penalty(nv, model = "rw2"))
  expect_snapshot(print(pen_cw2), variant = "print")
  expect_equal(dim(pen_cw2), c(n,n)) 
  expect_snapshot(print(as.matrix(pen_cw2)), variant ="matrix")
  #should have a nullspace rank of 2 and no negative eigenvalues
  #variant including derivatives
  expect_silent(pen_cw2_d <- mrf_penalty(nv, model = "rw2_d"))
  expect_snapshot(print(pen_cw2_d), variant = "print")
  expect_equal(dim(pen_cw2_d), c(2*n,2*n)) 
  expect_equal(get_labels(pen_cw2_d), c(1:n, paste0("d_", 1:n)))
  expect_snapshot(print(as.matrix(pen_cw2_d)), variant ="matrix")
})

test_that("rank properties of sequential rw2 penalties",{
  pen_cw2 <- mrf_penalty(nv, model = "rw2")
  pen_cw2_d <- mrf_penalty(nv, model = "rw2_d")
  pen_cw2_eigs <- eigen(pen_cw2,only.values = TRUE)$values
  expect_true(min(pen_cw2_eigs) > -eigtol)
  expect_equal(sum(pen_cw2_eigs > eigtol), n - 2)
  #should have a nullspace rank of 2 and no negative eigenvalues
  pen_cw2_d_eigs <- eigen(pen_cw2_d,only.values = TRUE)$values
  expect_true(min(pen_cw2_d_eigs) > -eigtol)
  expect_equal(sum(pen_cw2_d_eigs > eigtol), 2*n - 2)
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
    mrf_penalty(tr4,model = "ou")
    )
  expect_silent(pen4 <- mrf_penalty(tr4,model = "ou", alpha = 1))
  expect_snapshot(print(as.matrix(pen4)), variant = "matrix")
  
})

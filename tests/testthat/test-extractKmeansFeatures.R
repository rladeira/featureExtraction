
context("extractKmeansFeatures")

test_that("extractKmeansFeatures doesn't return NULL.", {
  
  data(iris)
  
  X <- iris[,1:4]
  
  kMeansFeatures <- extractKmeansFeatures(X, verbose = FALSE)
  expect_false(is.null(kMeansFeatures))
})

test_that("extractKmeansFeatures returns correct number of features.", {
  
  data(iris)
  
  X <- iris[,1:4]
  kValues <- 1:15
  
  kMeansFeatures <- extractKmeansFeatures(X, centers = kValues, verbose = FALSE)
  expect_true(ncol(kMeansFeatures) == length(kValues))
})


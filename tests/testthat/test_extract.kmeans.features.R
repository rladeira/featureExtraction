
context("extract.kmeans.features")

test_that("extract.kmeans.features doesn't return NULL.", {
  
  data(iris)
  
  X <- iris[,1:4]
  
  kmeans.features <- extract.kmeans.features(X, verbose = FALSE)
  expect_false(is.null(kmeans.features))
})

test_that("extract.kmeans.features returns correct number of features.", {
  
  data(iris)
  
  X <- iris[,1:4]
  k <- 1:15
  
  kmeans.features <- extract.kmeans.features(X, centers = k, verbose = FALSE)
  expect_true(ncol(kmeans.features) == length(k))
})



context("knnDistFeatures")

test_that("knnDistFeatures doesn't return NULL", {
  
  data(iris)
  
  n <- nrow(iris)
  X <- iris[,1:4]
  y <- iris[,5]
  
  train <- sample(n, n/2)
  
  knnDist <- knnDistFeatures(X[train,], y[train])
  expect_false(is.null(knnDist))
  
  features <- predict(knnDist, X[-train,])
  expect_false(is.null(features))
})

test_that("knnDistFeatures returns correct number of features", {
  
  data(iris)
  
  n <- nrow(iris)
  X <- iris[,1:4]
  y <- iris[,5]
  
  train <- sample(n, n/2)
  nClasses <- nlevels(y)
  k <- sample(2:10, 1)
  
  knnDist <- knnDistFeatures(X[train,], y[train], k)
  
  features <- predict(knnDist, X[-train,])
  expect_true(ncol(features) == (k * nClasses))
})

test_that("knnDistFeatures returns correct values", {
  
  X <- matrix(c(rep(0,10), rep(1, 10), rep(2, 10), rep(3, 10),
                rep(4,10), rep(5, 10), rep(6, 10), rep(7, 10),
                rep(8,10), rep(9, 10), rep(10, 10), rep(11, 10)),
              nrow = 12, ncol = 10, byrow = TRUE)
  
  y <- as.factor(c(1,1,1,2,2,2,1,1,1,2,2,2))
  train <- 1:6
  
  euclidDistance <- function(x1, x2) sqrt(sum((x1 - x2)^2))
  
  knnDist <- knnDistFeatures(X[train,], y[train], k = 2)
  
  features <- predict(knnDist, X[-train,])
  
  expectedDistancesTest <- c(euclidDistance(X[7,], X[3,]),
                             euclidDistance(X[7,], X[3,]) + euclidDistance(X[7,], X[2,]),
                             euclidDistance(X[7,], X[6,]),
                             euclidDistance(X[7,], X[6,]) + euclidDistance(X[7,], X[5,]))
  expect_true(all(expectedDistancesTest == features[1,]))
})



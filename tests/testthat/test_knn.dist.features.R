
context("knn.dist.features")

test_that("knn.dist.features doesn't return NULL", {
  
  data(iris)
  
  n <- nrow(iris)
  X <- iris[,1:4]
  y <- iris[,5]
  
  train <- sample(n, n/2)
  
  knn.dist <- knn.dist.features(X[train,], y[train], k = 10)
  expect_false(is.null(knn.dist))
  
  features <- predict(knn.dist, X[-train,])
  expect_false(is.null(features))
})

test_that("knn.dist.features returns correct number of features", {
  
  data(iris)
  
  n <- nrow(iris)
  X <- iris[,1:4]
  y <- iris[,5]
  
  train <- sample(n, n/2)
  n.classes <- nlevels(y)
  k <- sample(5:10, 4)
  
  knn.dist <- knn.dist.features(X[train,], y[train], k)
  
  features <- predict(knn.dist, X[-train,])
  expect_true(ncol(features) == (length(k) * n.classes))
})

test_that("knn.dist.features returns correct values", {
  
  X <- matrix(c(rep(0,10), rep(1, 10), rep(2, 10), rep(3, 10),
                rep(4,10), rep(5, 10), rep(6, 10), rep(7, 10),
                rep(8,10), rep(9, 10), rep(10, 10), rep(11, 10)),
              nrow = 12, ncol = 10, byrow = TRUE)
  
  y <- as.factor(c(1,1,1,2,2,2,1,1,1,2,2,2))
  train <- 1:6
  
  euclid.distance <- function(x1, x2) sqrt(sum((x1 - x2)^2))
  
  knn.dist <- knn.dist.features(X[train,], y[train], k = 1:2)
  
  features <- predict(knn.dist, X[-train,])
  
  expected.distances <- c(euclid.distance(X[7,], X[3,]),
                          (euclid.distance(X[7,], X[3,]) + euclid.distance(X[7,], X[2,]))/2,
                          euclid.distance(X[7,], X[6,]),
                          (euclid.distance(X[7,], X[6,]) + euclid.distance(X[7,], X[5,]))/2)
  expect_true(all(expected.distances == features[1,]))
})



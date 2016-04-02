
context("occurrence.encoding")

test_that("occurrence.encoding doesn't return NULL.", {
  
  data(iris)
  
  data <- occurrence.encoding(iris, features = colnames(iris), verbose = FALSE)
  
  expect_false(is.null(data))
})

test_that("occurrence.encoding returns correct encoding 1.", {
  
  data(iris)
  
  data <- occurrence.encoding(iris, features = colnames(iris), verbose = FALSE)
  
  expect_true(all(data$Species == 50))
})

test_that("occurrence.encoding returns correct encoding 2.", {
  
  data <- data.frame(feature1 = c(rep(1, 10), rep(2, 20),
                                  rep(3, 30), rep(4, 40)))
  
  encoded.data <- occurrence.encoding(data, features = colnames(data), verbose = FALSE)
  
  expected.encoding <- c(rep(10, 10), rep(20, 20),
                         rep(30, 30), rep(40, 40))
  
  expect_true(all(expected.encoding == encoded.data[[1]]))
})



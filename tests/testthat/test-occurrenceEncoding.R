
context("occurrenceEncoding")

test_that("occurrenceEncoding doesn't return NULL.", {
  
  data(iris)
  
  data <- occurrenceEncoding(iris, features = colnames(iris), verbose = FALSE)
  
  expect_false(is.null(data))
})

test_that("occurrenceEncoding returns correct encoding 1.", {
  
  data(iris)
  
  data <- occurrenceEncoding(iris, features = colnames(iris), verbose = FALSE)
  
  expect_true(all(data$Species == 50))
})

test_that("occurrenceEncoding returns correct encoding 2.", {
  
  data <- data.frame(feature1 = c(rep(1, 10), rep(2, 20),
                                  rep(3, 30), rep(4, 40)))
  
  encodedData <- occurrenceEncoding(data, features = colnames(data), verbose = FALSE)
  
  expectedEncoding <- c(rep(10, 10), rep(20, 20),
                        rep(30, 30), rep(40, 40))
  
  expect_true(all(expectedEncoding == encodedData[[1]]))
})



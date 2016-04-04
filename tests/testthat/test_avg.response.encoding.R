
context("avg.response.encoding")

test_that("avg.response.encoding doesn't return NULL.", {
  
  data(iris)
  
  enc.obj <- avg.response.encoding(data = iris[,-5],
                                   label = iris[,5],
                                   features = colnames(iris)[-5])
  expect_false(is.null(enc.obj))
})

test_that("avg.response.encoding returns correct encoding 1.", {
  
  data <- data.frame(f1 = c(rep(1,4), rep(2,6)))
  label <- c(rep(0,5), rep(1,5))
  
  enc.obj <- avg.response.encoding(data, label, "f1")
  encodings <- predict(enc.obj, data, verbose = FALSE)
  
  expect_true(all(encodings[1:4,] == 0))
  expect_true(all(encodings[5:10,] == 5/6))
})

test_that("avg.response.encoding returns correct encoding 2.", {
  
  data <- data.frame(f1 = c(rep(1,55), rep(2,45)))
  label <- c(rep(0,20), rep(1,80))
  
  enc.obj <- avg.response.encoding(data, label, "f1")
  encodings <- predict(enc.obj, data, verbose = FALSE)
  
  expect_true(all(encodings[1:55,] == 35/55))
  expect_true(all(encodings[56:100,] == 1))
  
  new.data <- data.frame(f1 = c(rep(1,10),rep(2,10)))
  encodings <- predict(enc.obj, new.data, verbose = FALSE)
  
  expect_true(all(encodings[1:10,] == 35/55))
  expect_true(all(encodings[11:20,] == 1))
})



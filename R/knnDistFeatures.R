
library(plyr)
library(FNN)

knnDistFeatures <- function(x, ...) UseMethod("knnDistFeatures")

knnDistFeatures.default <- function(x, y, k = 10, verbose = TRUE) 
{
  if(is.factor(y) == FALSE)
    stop("y argument must be a factor.")
  if(nrow(x) != length(y))
    stop("y vector's length should equal to the number of rows in x.")
  
  classes <- levels(y) 
  classIdxs <- lapply(
    classes, 
    function (class) list(class = class, idxs = which(y == class))) 
  
  if(verbose)
  {
    cat("\nExtracting features for sum of nearest neighbors...\n")
    progressBar <- create_progress_bar("text")
    progressBar$init(nlevels(y))
  }
  
  xKnnFeatures <- lapply(
    classIdxs,
    function (info)
    {
      class <- info$class
      classIdx <- info$idxs
      distances <- knnx.dist(data = x[classIdx,],
                             query = x, k = k+1, 
                             algorithm = "kd_tree")
      
      distanceSums <- Reduce(
        cbind, lapply(1:k, function (i)
        {
          apply(distances, 1, sumDistancesForTrainingData, k = i)
        }))
      distanceSums <- configureColNamesFor(distanceSums, class)
      
      if(verbose) progressBar$step()
      distanceSums
    })
  
  xKnnFeatures <- Reduce(cbind, xKnnFeatures)
  
  structure(
    list(x = x,
         y = y,
         xKnnFeatures = xKnnFeatures,
         classes = classes,
         classIdxs = classIdxs,
         k = k,
         verbose = verbose,
         call = match.call()),
    class = "knnDistFeatures"
  )
} 

predict.knnDistFeatures <- function(knnDistObj, newData,
                                    appendTrainFeatures = FALSE) 
{
  x <- knnDistObj$x
  k <- knnDistObj$k
  classIdxs <- knnDistObj$classIdxs
  verbose <- knnDistObj$verbose
  
  if(verbose) {
    cat("\nExtracting knn distance features...\n")
    progressBar <- create_progress_bar("text")
    progressBar$init(length(knnDistObj$classes))
  }
  
  newDataDistanceSums <- lapply(
    classIdxs,
    function (info)
    {
      class <- info$class
      classIdx <- info$idxs
      distances <- knnx.dist(data = x[classIdx,],
                             query = newData, k = k, 
                             algorithm = "kd_tree")
      
      distanceSums <- Reduce(
        cbind, lapply(1:k, function (i)
        {
          apply(distances, 1, function (row) sum(row[1:i]))
        }))
      distanceSums <- configureColNamesFor(distanceSums, class)
      
      if(verbose) progressBar$step()
      distanceSums
    })
  
  newDataFeatures <- Reduce(cbind, newDataDistanceSums)
  
  if(appendTrainFeatures)
  {
    features <- rbind(knnDistObj$xKnnFeatures, newDataFeatures)
    return(features)
  }
  
  newDataFeatures
}

sumDistancesForTrainingData <- function(row, k) {
  ifelse(row[1] == 0,
         sum(row[2:(k+1)]),
         sum(row[1:k]))
}

configureColNamesFor <- function(distanceSums, class)
{
  k <- length(colnames(distanceSums))
  colnames(distanceSums)[1] <- sprintf("nnDistance_class_%s_k_%d", class, 1)
  colnames(distanceSums)[2:k] <- sapply(
    2:k, function (i) sprintf("nnDistanceSum_class_%s_k_%d", class, i))
  
  distanceSums
}



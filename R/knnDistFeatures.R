
knnDistFeatures <- function(x, ...) UseMethod("knnDistFeatures")

knnDistFeatures.default <- function(x, y, k = 10) 
{
  if(is.factor(y) == FALSE)
    stop("y argument must be a factor.")
  if(nrow(x) != length(y))
    stop("y vector's length should equal to the number of rows in x.")
  
  classes <- levels(y) 
  classIdxs <- lapply(
    classes, 
    function (class) list(class = class, idxs = which(y == class))) 
  
  structure(
    list(x = x,
         y = y,
         classes = classes,
         classIdxs = classIdxs,
         k = k,
         call = match.call()),
    class = "knnDistFeatures"
  )
} 

predict.knnDistFeatures <- function(knnDistObj, newData) 
{
  x <- knnDistObj$x
  k <- knnDistObj$k
  classIdxs <- knnDistObj$classIdxs
  
  newDataDistanceSums <- lapply(
    classIdxs,
    function (info)
    {
      class <- info$class
      classIdx <- info$idxs
      distances <- FNN::knnx.dist(
        data = x[classIdx,],
        query = newData, k = k, 
        algorithm = "kd_tree")
      
      distanceSums <- Reduce(
        cbind, lapply(1:k, function (i)
        {
          apply(distances, 1, function (row) sum(row[1:i]))
        }))
      distanceSums <- configureColNamesFor(distanceSums, class)
      
      distanceSums
    })
  
  newDataFeatures <- Reduce(cbind, newDataDistanceSums)
  
  newDataFeatures
}

configureColNamesFor <- function(distanceSums, class)
{
  k <- length(colnames(distanceSums))
  colnames(distanceSums)[1] <- sprintf("nnDistance_class_%s_k_%d", class, 1)
  colnames(distanceSums)[2:k] <- sapply(
    2:k, function (i) sprintf("nnDistanceSum_class_%s_k_%d", class, i))
  
  distanceSums
}




knnDistFeatures <- function(x, ...) UseMethod("knnDistFeatures")

knnDistFeatures.default <- function(x, y, kValues = 1:10) 
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
         kValues = kValues,
         call = match.call()),
    class = "knnDistFeatures"
  )
} 

predict.knnDistFeatures <- function(knnDistObj, newData) 
{
  x <- knnDistObj$x
  kValues <- knnDistObj$kValues
  maxK <- max(kValues)
  classIdxs <- knnDistObj$classIdxs
  
  newDataMeanDistances <- lapply(
    classIdxs,
    function (info)
    {
      class <- info$class
      classIdx <- info$idxs
      distances <- FNN::knnx.dist(
        data = x[classIdx,],
        query = newData, k = maxK, 
        algorithm = "kd_tree")
      
      meanDistances <- as.matrix(Reduce(
        cbind, lapply(kValues, function (i)
        {
          apply(distances, 1, function (row) sum(row[1:i])/i)
        })))
      meanDistances <- configureColNamesFor(meanDistances, kValues, class)
      
      meanDistances
    })
  
  newDataFeatures <- Reduce(cbind, newDataMeanDistances)
  
  newDataFeatures
}

configureColNamesFor <- function(meanDistances, kValues, class)
{
  i <- 1
  for(k in kValues)
  {
    colnames(meanDistances)[i] <- sprintf("nnMeanDistance_class_%s_k_%d", class, k)
    i <- i + 1
  }
  meanDistances
}




knn.dist.features <- function(x, ...) UseMethod("knn.dist.features")

knn.dist.features.default <- function(x, y, k = c(1,2,4)) 
{
  if(is.factor(y) == FALSE)
    stop("y argument must be a factor.")
  if(nrow(x) != length(y))
    stop("y vector's length should equal to the number of rows in x.")
  
  classes <- levels(y) 
  class.idxs <- lapply(
    classes, 
    function (class) list(class = class, idxs = which(y == class))) 
  
  structure(
    list(x = x,
         y = y,
         classes = classes,
         class.idxs = class.idxs,
         k = k,
         call = match.call()),
    class = "knn.dist.features"
  )
} 

predict.knn.dist.features <- function(knn.dist.obj, new.data) 
{
  x <- knn.dist.obj$x
  k <- knn.dist.obj$k
  max.k <- max(k)
  class.idxs <- knn.dist.obj$class.idxs
  
  new.data.mean.distances <- lapply(
    class.idxs,
    function (info)
    {
      class <- info$class
      class.idx <- info$idxs
      distances <- FNN::knnx.dist(
        data = x[class.idx,],
        query = new.data, k = max.k, 
        algorithm = "kd_tree")
      
      mean.distances <- as.matrix(Reduce(
        cbind, lapply(k, function (i)
        {
          apply(distances, 1, function (row) sum(row[1:i])/i)
        })))
      mean.distances <- configure.colnames.for(mean.distances, k, class)
      
      mean.distances
    })
  
  new.data.features <- Reduce(cbind, new.data.mean.distances)
  
  new.data.features
}

configure.colnames.for <- function(mean.distances, k, class)
{
  i <- 1
  for(k in k)
  {
    colnames(mean.distances)[i] <- sprintf("nnMeanDistance_class_%s_k_%d", class, k)
    i <- i + 1
  }
  mean.distances
}



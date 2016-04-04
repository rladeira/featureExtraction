
knn.dist.features <- function(x, ...) UseMethod("knn.dist.features")

knn.dist.features.default <- function(data, label, k = c(1,2,4)) 
{
  if(is.factor(label) == FALSE)
    stop("label argument must be a factor.")
  if(nrow(data) != length(label))
    stop("label vector's length should equal to the number of rows in data.")
  
  classes <- levels(label) 
  class.idxs <- lapply(
    classes, 
    function (class) list(class = class, idxs = which(label == class))) 
  
  structure(
    list(data = data,
         label = label,
         classes = classes,
         class.idxs = class.idxs,
         k = k,
         call = match.call()),
    class = "knn.dist.features"
  )
} 

predict.knn.dist.features <- function(knn.dist.obj, new.data) 
{
  data <- knn.dist.obj$data
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
        data = data[class.idx,],
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



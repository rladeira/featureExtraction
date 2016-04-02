
library(plyr)
library(stats)

extract.kmeans.features <- function(data, centers = 2:10, iter.max = 10,
                                    nstart = 1, kmeans.trace = FALSE,
                                    verbose = TRUE) 
{
  if(verbose)
  {
    cat("\nExtracting kmeans features...\n")
    progress.bar <- create_progress_bar("text")
    progress.bar$init(length(centers))
  }
  
  kmeans.features <- sapply(
    centers,
    function (k) {
      kmeans.result <- kmeans(data, centers = k, 
                              iter.max = iter.max,
                              nstart = nstart, 
                              trace = kmeans.trace)
      if(verbose) progress.bar$step()
      kmeans.result$cluster
    })
  
  colnames(kmeans.features) <- sapply(centers, function (k) sprintf("kmeans_%d", k))
  
  kmeans.features
}
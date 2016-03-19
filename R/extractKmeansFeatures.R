
library(plyr)
library(stats)

extractKmeansFeatures <- function(data, centers = 2:10, iter.max = 10,
                                  nstart = 1, kMeansTrace = FALSE,
                                  verbose = TRUE) 
{
  if(verbose)
  {
    cat("\nExtracting kmeans features...\n")
    progressBar <- create_progress_bar("text")
    progressBar$init(length(centers))
  }
  
  kMeansfeatures <- sapply(
    centers,
    function (k) {
      kmeansResult <- kmeans(data, centers = k, 
                             iter.max = iter.max,
                             nstart = nstart, 
                             trace = kMeansTrace)
      if(verbose) progressBar$step()
      kmeansResult$cluster
    })
  
  colnames(kMeansfeatures) <- sapply(centers, function (k) sprintf("kmeans_%d", k))
  
  kMeansfeatures
}
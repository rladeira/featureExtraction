
library(plyr)
library(dplyr)
library(stats)

extractKmeansFeatures <- function(data, centers = 2:10) {
  
  cat("\nExtracting kmeans features...\n")
  
  progressBar <- create_progress_bar("text")
  progressBar$init(length(centers))
  
  features <- sapply(
    centers,
    function (k) {
      kmeansResult <- kmeans(data, centers = k)
      progressBar$step()
      kmeansResult$cluster
    })
  
  colnames(features) <- sapply(centers, function (k) sprintf("kmeans_%d", k))
  
  features
}
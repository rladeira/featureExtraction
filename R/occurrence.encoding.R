
occurrence.encoding <- function(data, features, verbose = TRUE)
{
  if (is.data.frame(data) == FALSE) {
    data <- as.data.frame(data)
  }
  
  library(hash)
  
  if (verbose)
  {
    library(plyr)
    
    cat("Extracting features by occurrence encoding...\n")
    progress.bar <- create_progress_bar("text")
    progress.bar$init(length(features))
  }
  
  feature.idxs <- which(colnames(data) %in% features)
  for (i in feature.idxs)
  {
    data[[i]] <- as.numeric(data[[i]])
    occurrence.table <- table(data[[i]])
    map <- hash(keys = names(occurrence.table),
                values = as.numeric(occurrence.table))
    
    for (key in names(occurrence.table)) {
      data[[i]][data[[i]] == key] <- map[[key]]
    }
    if (verbose) {
      progress.bar$step()
    }
  }
  if (verbose) {
    cat("\n")
  }
  
  data
}
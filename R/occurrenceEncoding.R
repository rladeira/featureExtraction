
occurrenceEncoding <- function(data, features, verbose = TRUE)
{
  if (is.data.frame(data) == FALSE)
  {
    data <- as.data.frame(data)
  }
  
  library(hash)
  
  if (verbose)
  {
    library(plyr)
    
    cat("Extracting features by occurrence encoding...\n")
    progressBar <- create_progress_bar("text")
    progressBar$init(length(features))
  }
  
  for (feature in features)
  {
    data[[feature]] <- as.numeric(data[[feature]])
    occurrenceTable <- table(data[[feature]])
    map <- hash(keys = names(occurrenceTable),
                values = as.numeric(occurrenceTable))
    
    for (key in names(occurrenceTable))
    {
      data[[feature]][data[[feature]] == key] <- map[[key]]
    }
    
    if (verbose) progressBar$step()
  }
  if (verbose) cat("\n")
  
  data
}
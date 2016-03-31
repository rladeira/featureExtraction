
occurrenceEncoding <- function(data, features)
{
  if (is.data.frame(data) == FALSE)
  {
    data <- as.data.frame(data)
  }
  
  library(hash)
  
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
  }
  
  data
}
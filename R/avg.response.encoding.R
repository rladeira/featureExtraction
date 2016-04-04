
avg.response.encoding <- function(x, ...) UseMethod("avg.response.encoding")

avg.response.encoding.default <- function(data, label, features, verbose = FALSE)
{
  if (is.data.frame(data) == FALSE) {
    data <- as.data.frame(data)
  }
  if (is.numeric(label) == FALSE) {
    label <- as.numeric(label)
  }
  
  library(plyr)
  library(dplyr)
  library(hash)
  
  if (verbose)
  {
    cat("Creating encoding map...\n")
    progress.bar <- create_progress_bar("text")
    progress.bar$init(length(features))
  }
  
  encoding.map <- hash()
  feature.idxs <- which(colnames(data) %in% features)
  for (i in feature.idxs)
  {
    df <- data.frame(feature = data[[i]],
                     label = label)
    groupings <- df %>%
      group_by(feature) %>%
      summarise(avg = mean(label))
    
    encoding.map[[make.keys(i)]] <- hash(
      keys = make.keys(groupings$feature),
      values = groupings$avg)
    
    if (verbose) progress.bar$step()
  }
  if (verbose) cat("\n")
  
  structure(
    list(encoding.map = encoding.map,
         feature.idxs = feature.idxs,
         call = match.call()),
    class = "avg.response.encoding"
  )
}

predict.avg.response.encoding <- function(enc.obj, new.data, verbose = TRUE)
{
  if (is.data.frame(new.data) == FALSE)
  {
    new.data <- as.data.frame(new.data)
  }
  
  feature.idxs <- enc.obj$feature.idxs
  encoding.map <- enc.obj$encoding.map
  
  if (verbose)
  {
    cat("Extracting average response encoding...\n")
    progress.bar <- create_progress_bar("text")
    progress.bar$init(length(feature.idxs))
  }
  
  for (i in feature.idxs)
  {
    map <- encoding.map[[make.keys(i)]]
    for(key in keys(map))
    {
      new.data[[i]] <- make.keys(new.data[[i]])
      new.data[[i]][new.data[[i]] == key] <- map[[key]]
    }
    if (verbose) {
      progress.bar$step()
    }
  }
  if (verbose) {
    cat("\n")
  }
  new.data
}





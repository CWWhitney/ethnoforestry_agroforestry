log_dropped_docs <- function(corpus, transformation, description) {
  # Get initial lengths
  initial_lengths <- sapply(corpus, function(doc) nchar(as.character(doc)))
  
  # Apply transformation
  corpus <- tm_map(corpus, transformation)
  
  # Get new lengths
  new_lengths <- sapply(corpus, function(doc) nchar(as.character(doc)))
  
  # Identify dropped documents
  dropped_docs <- which(new_lengths == 0 & initial_lengths > 0)
  
  if (length(dropped_docs) > 0) {
    cat(paste("Dropped", length(dropped_docs), "documents after:", description, "\n"))
    cat("Dropped document indices:", paste(dropped_docs, collapse = ", "), "\n")
  } else {
    cat(paste("No documents dropped after:", description, "\n"))
  }
  
  return(corpus)
}

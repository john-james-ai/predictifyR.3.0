## ---- chunk_corpus
#==============================================================================#
#                           chunkDocument                                      #
#==============================================================================#
#'  chunkDocument
#'
#' This function takes as its parameters, a document in unlisted sentence or
#' word tokenized format and the chunk size in terms of numbers of tokens,
#' and returns the document in a list of chunks of equal size.
#'
#' @param document - the document to be sampled
#' @param chunkSize - the number of sentences per chunk
#' @return chunks - list of chunks of equal number of sentences
#' @author John James
#' @export
chunkDocument <- function(document, chunkSize) {

  # Validate inputs
  docLength <-length(document)
  stopifnot(chunkSize <= docLength)

  # Set num chunks
  numChunks <- floor(length(document) / chunkSize)

  # Break file into chunks
  chunks <- list()
  start <- 1
  end <- min(length(document), chunkSize)

  for (i in 1:numChunks) {
    start <- chunkSize * (i - 1) + 1
    end <- start + chunkSize - 1
    chunks[[i]] <- document[start:end]
  }

  # Last Chunk
  if (end < length(document)) {
    start <- end + 1
    chunks[[i+1]] <- document[start:length(document)]
  }
  return(chunks)
}
## ---- end

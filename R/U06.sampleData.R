## ---- sample_data
#==============================================================================#
#                           sampleData                                         #
#==============================================================================#
#'  sampleData
#'
#'  \code{sampleData} Returns samples from a corpus document
#'
#' This function takes as its parameters, a document in unlisted tokenized
#' format, the number of chunks, the chunk size, and the format of the returned
#' document, then returns a sample document according to parameters.
#'
#' @param document Vector containing a text document
#' @param numChunks Integer indicating number of chunks or samples to return
#' @param chunkSize Integer indicating the size chunk/sample size in percent or
#'                  number of tokens
#' @param format - format of the data to be returned values are:
#'            lv = list of character vectors
#'            ls = list of character strings
#'            v = unlisted character vector
#'            s = unlist character string
#' @return samples - the samples in a contiguous vector of tokens
#' @author John James
#' @export
sampleData <- function(document, numChunks, chunkSize, format = 'lv') {

  # Initialize Seed
  set.seed(007)

  # Validate inputs
  docLength <-length(document)
  if (numChunks * chunkSize > docLength) {
    futile.logger::flog.error(
      paste0("Error in Sample Data. Number of chunks(",numChunks,
            ") times chunk size (", chunkSize, ') is greater than document size(',
            docLength,")"), name = 'red')
    stop()
  }

  # Sample Data
  start        <- sample(1:(docLength-chunkSize), numChunks)
  end          <- start + chunkSize - 1
  samples <- lapply(seq(1:numChunks), function(x) {
    document[start[x]:end[x]]
  })

  # Format output
  if (format == 'ls' | format == 's') {
    samples <- lapply(seq_along(samples), function(x) {
      paste(samples[[x]], collapse = ' ')
    })
  }

  if (format == 'v' | format == 's') {
    samples <- unlist(samples)
  }

  return(samples)
}
## ---- end

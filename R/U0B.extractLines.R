## ---- extract_lines

#==============================================================================#
#                              extractLines                                    #
#==============================================================================#
#'  extractLines
#'
#'  \code{extractLines} removes text lines which include designated strings
#'
#' This function takes as its parameters, a list or vector of texts and a
#' vector of strings to be excluded from the text, then removes the lines
#' containing the string(s) and returns the cleaned document
#' to the calling environment.
#'
#' @param document List of texts to be cleaned
#' @param strings Vector of strings to be removed
#' @return cleanDocument List of text without lines containing profanity
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family text processing functions
extractLines <- function(document, strings) {

  # Validate input
  if (class(document) != 'character') {
    futile.logger::flog.error('Error in extractLines. Document is not
                              character class',
                              name = 'red')
    stop()
  } else if (length(document) == 0) {
    futile.logger::flog.error('Error in extractLines. Document is empty',
                              name = 'red')
    stop()
  } else if (length(strings) == 0) {
    futile.logger::flog.error('Error in extractLines. String vector is empty',
                              name = 'red')
    stop()
  } else if (class(strings) != 'character') {
    futile.logger::flog.error('Error in extractLines. String is not
                              character class',
                              name = 'red')
    stop()
  }

  stringsRegex <- paste0("\\b",strings, "\\b", collapse = '|')
  xidx <- unique(grep(stringsRegex, document, ignore.case = TRUE))
  document <- document[-xidx]
  return(document)
}

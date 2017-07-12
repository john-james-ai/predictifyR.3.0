## ---- read_file
#==============================================================================#
#                                 readFile                                     #
#==============================================================================#
#'  readFile
#'
#'  \code{readFile} reads a csv, dic, or text file into memory
#'
#' The function takes a directory and file name as s parameter, reads the
#' file and returns the object to the calling environment.
#'
#' @param object List containing directory and filename
#' @return fileData The csv, dic, or text data
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family io functions
#' @export
readFile <- function(object) {

  directory <- object$directory
  file      <- object$fileName

  if (file_ext(file) == 'txt' | file_ext(file) == 'dic') {
    fileData <- readLines(file.path(directory, file))
  } else {
    if (file_ext(file) == 'csv') {
      fileData <- read.csv(file.path(directory, file),
                           header = TRUE,
                           stringsAsFactors = FALSE)
    }
  }

  return(fileData)
}

## ---- end

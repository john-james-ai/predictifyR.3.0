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
  fileName  <- object$fileName
  filePath  <- file.path(directory, fileName)

  # Error handling
  if (!file.exists(filePath)) {
    futile.logger::flog.error(paste('Error in readFile.', filePath, 'does not exist'), name = 'red')
    stop()
  } else if (!(tools::file_ext(fileName) %in% c('txt', 'dic', 'csv'))) {
    futile.logger::flog.error(paste('Error in readFile.
                     Function valid for txt, dic, and csv files only'),
               name = 'red')
    stop()
  }

  # Establish connection
  con <- file(file.path(directory, fileName))
  on.exit(close(con))

  # Read file
  if (tools::file_ext(fileName) == 'txt' | tools::file_ext(fileName) == 'dic') {
    fileData <- readLines(con)
  } else if (tools::file_ext(file) == 'csv') {
      fileData <- read.csv(con, header = TRUE, stringsAsFactors = FALSE)
  }

  return(fileData)
}

## ---- end

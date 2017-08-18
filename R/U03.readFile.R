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
#' @param binary Logical. Read binary if TRUE
#' @return fileData The csv, dic, bin, or text data
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family io functions
#' @export
readFile <- function(object, binary = FALSE) {

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
  if (binary == TRUE) {
    fileData <- readBin(file.path(directory, fileName),
                        raw(), file.info(file.path(directory, fileName))$size)
  } else  if (tools::file_ext(fileName) == 'txt' | tools::file_ext(fileName) == 'dic') {
    fileData <- readLines(con)
  } else if (tools::file_ext(file) == 'csv') {
      fileData <- read.csv(con, header = TRUE, stringsAsFactors = FALSE)
  }

  return(fileData)
}

## ---- end

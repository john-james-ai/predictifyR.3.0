## ---- save_file
#==============================================================================#
#                             saveFile                                         #
#==============================================================================#
#' saveFile
#'
#' \code{saveFile} saves a csv, dic, or text file to disk
#'
#' This function function takes as a parameter, file and meta data and
#' saves the file in accordance with the file meta data (directory, filename).
#'
#' @param object - a list containing:
#' \itemize{
#'    \item{directory}{Directory into which the data is to be stored}
#'    \item{fileName}{File name given to the file to be saved}
#'    \item{data}{File contents}#'
#'    }
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family io functions
#' @export
saveFile <- function(object) {

  # Create directory if necessary
  checkDir(object$directory)

  # Assign file path
  filePath  <- file.path(object$directory, object$fileName)

  # Save file
  if (tools::file_ext(object$fileName) == 'txt' | tools::file_ext(object$fileName) == 'dic') {
    con <- file(filePath)
    writeLines(object$data, con)
    close(con)
  } else {
    if (tools::file_ext(object$fileName) == 'csv') {
      write.csv(x = object$data, file = filePath, row.names = FALSE)
    }
  }
}
## ---- end

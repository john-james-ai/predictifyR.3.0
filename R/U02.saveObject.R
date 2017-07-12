## ---- save_object
#==============================================================================#
#                             saveObject                                       #
#==============================================================================#
#' saveObject
#'
#' \code{saveObject} saves an R object in rda format.
#'
#' This function function takes as a parameter, a list containing both data and
#' meta data (directory, object name, filename), and saves the data in an
#' RDA file according to the metaData.
#'
#' @param object List containing:
#' \itemize{
#'    \item{directory}{Directory into which the data is to be stored}
#'    \item{objName}{Object name given to the R object created}
#'    \item{fileName}{File name for the R object created}
#'    }
#' @return object - R object saved
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family io functions
#' @export
saveObject <- function(object) {

  # Unpack instructions
  directory  <- object$directory
  objName    <- object$objName
  fileName   <- object$fileName

  # Create directory if necessary
  checkDir(directory)

  # Assign analysis object to custom name
  assign(objName, object$data)

  # Assign file path
  filePath  <- file.path(directory, fileName)

  # Save file
  save(list=objName, file = filePath)

}

## ---- end

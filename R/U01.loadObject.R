## ---- load_object
#==============================================================================#
#                             loadObject                                       #
#==============================================================================#
#' loadObject
#'
#'  \code{loadObject} loads an R object from an RDA file into memory
#'
#' This function takes as its parameter a list containing a directory and
#' filename. It loads the associated object into memory and returns it to the
#' calling environment.
#'
#' @param object A list containing a directory and filename
#' @return object R object loaded
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family io functions
#' @export
loadObject <- function(object) {

  # Unpack instructions
  directory  <- object$directory
  fileName   <- object$fileName

  # Create file path
  filePath <- file.path(directory, fileName)

  if (file.exists(filePath)) {

    # load object
    env <- new.env()
    object <- load(filePath, envir = env)
    return(env[[object]])
  } else {
    futile.logger::flog.error(paste('Error in loadObject.
                     File', file.path(directory, fileName), 'does not exist.'),
                              name = 'red')
    stop()
  }
}
## ---- end

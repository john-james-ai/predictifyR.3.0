logResults <- function(result, fileName, objName, text = FALSE) {

  object <- list()
  object$directory <- file.path(directories$testingDir, 'results')
  object$fileName <- fileName
  object$objName <- objName
  object$data <- result
  if (text) {
    saveFile(object)
  } else {
    saveObject(object)
  }
}

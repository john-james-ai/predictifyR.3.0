logResults <- function(result, fileName, objName) {
  object <- list()
  object$directory <- file.path(directories$testingDir, 'results')
  object$fileName <- fileName
  object$objName <- objName
  object$data <- result
  saveObject(object)
}

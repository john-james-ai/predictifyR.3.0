testConditioner <- function(rawDoc) {
  document <- conditionDocument(rawDoc)
  logResults(document, rawDoc$fileName, rawDoc$objName, text = TRUE)
}

document <- testConditioner(corpora$raw$documents[[2]])

createTestCorpus <- function(raw, test) {
  lapply(seq_along(raw$documents), function(x) {
    document <- readFile(raw$documents[[x]])
    document <- sample(document, size = .01 * length(document))
    test$documents[[x]]$data <- document
    saveFile(test$documents[[x]])
  })
}
createTestCorpus(raw, corpora$test)

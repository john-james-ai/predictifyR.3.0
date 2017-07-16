## ---- estimate_corpus_size
#------------------------------------------------------------------------------#
#                            estimateCorpusSize                                #
#------------------------------------------------------------------------------#
#'  estimateCorpusSize
#'
#'  \code{estimateCorpusSize} Estimates corpus size based upon lexical features
#'
#' This function takes as its parameters, the korpus meta data and the POS tags
#' selected for this analysis, the returns an estimate of total corpus size
#' based upon the distribution of lexical  features per n000-word samples of
#' the text. This analysis is based upon Representativeness in Corpus Design
#' Biber 1993
#' \url{https://www.researchgate.net/publication/31460364_Representativeness_in_Corpus_Design}
#'
#' @param korpus List containing the corpus meta data
#' @param sampleSize Numeric indicating the sampling unit size
#' @param numSamples Numeric indicating the number of samples to analyize
#' @return corpusSize List cointaining:
#' \itemize{
#'  \item{n}{Corpus sample size estimate}
#'  \item{posAnalysis}{POS tag distribution analysis}
#' }
#'
#' @family sample size estimate functions
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @seealso
#'   \code{\link{analyzeLexicalFeatures}}
#' @export
estimateCorpusSize <- function(korpus, sampleSize = 2000,
                               numSamples = 100) {

  startTime <- Sys.time()
  futile.logger::flog.info("Estimating total corpus size",
                           name = 'green')

  # Designate POS tags to use in the study
  posTags <- subset(posTags, Study == TRUE)

  # Read and combine corpus into single document
  futile.logger::flog.info("...loading corpus files", name = 'green')
  document <- unlist(lapply(seq_along(korpus$documents), function(x) {
    readFile(korpus$documents[[x]])
  }))

  # Convert to word tokens
  futile.logger::flog.info("...tokenizing corpus files", name = 'green')
  document <- unlist(quanteda::tokenize(document, what = 'word'))

  # Split data into chunks
  futile.logger::flog.info(paste0('...sampling ', numSamples, ', ',
                                  sampleSize, '-word samples'),
                           name = 'green')
  chunks <- sampleData(document, numChunks = numSamples,
                       chunkSize = sampleSize, format = 'lv')

  # Prepare tag analysis
  futile.logger::flog.info("...conducting POS analysis",
                           name = 'green')
  posAnalysis <- analyzeLexicalFeatures(chunks)

  # Corpus size estimate is the largest n not including rare tags (< 1%) of total tags
  wordsTagged <- sum(posAnalysis$featureStats$total)
  n  <- round(max(subset(posAnalysis$featureStats, select = n)$n), 0)

  # Format result
  corpusSize <- list(
    n = n,
    analysis = posAnalysis$featureStats
  )

  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(
    paste('Corpus sample size estimate complete.  Elapsed time is',
          format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')

  return(corpusSize)
}
## ---- end

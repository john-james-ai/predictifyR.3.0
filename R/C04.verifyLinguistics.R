## ---- verify_corpus_features

#==============================================================================#
#                          verifyCorpusLexicalFeatures                         #
#==============================================================================#
#'  verifyCorpusLexicalFeatures
#'
#'  \code{getCorpus} Verifies lexical features distribution of a corpus
#'
#' This function takes as its parameters, a list containing the korpus
#' meta data, and two integers, one indicating the number of chunks to sample
#' the other indicating the size of each chunk in sentences. The function then
#' reads, samples, and analyzes the lexical feature distribution for the
#' corpus.
#'
#' @param korpus List containing meta data for the corpus
#' @param chunks Integer indicating number of chunks to sample
#' @param chunkSize Integer indicating sizes of chunk in sentences
#' @return analysis - the lexical feature distribution analysis.
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
verifyCorpusLexicalFeatures <- function(korpus, chunks, chunkSize) {

  futile.logger::flog.info(paste('...loading', korpus$corpusName),
                           name = 'green')
  document <- unlist(lapply(seq_along(korpus$documents), function(d) {
    unlist(tokenize(readFile(korpus$documents[[d]]), what = 'word'))
  }))

  futile.logger::flog.info(paste('...sampling', korpus$corpusName),
                           name = 'green')
  samples <- sampleData(document, numChunks = chunks,
                        chunkSize = chunkSize, format = 'lv')

  futile.logger::flog.info('...conducting lexical feature analysis',
                           name = 'green')
  analysis <-  analyzeLexicalFeatures(samples)

  return(analysis)

}



#==============================================================================#
#                              verifyLinguistics                               #
#==============================================================================#
#'  verifyLinguistics
#'
#'\code{getCorpus} Compares distribution of lexical features between 2 corpora
#'
#' This function takes as its parameters, the meta data for the full corpus
#' and the sample corpus and returns comparative statistics of the
#' distribution of lexical features.
#'
#' @param fullCorpus List containing the meta data for the HC Corpus
#' @param sampleCorpus  List containing the meta data for the sampleCorpus corpus
#' @param chunkSize The number of sentences in each chunk sampled
#' @param chunks The number of chunks to sample
#' @return features A list containing:
#' \itemize{
#'  \item{analyses}{Lexical feature analysis for both corpora}
#'  \item{means}{The mean numbers of features per sample}
#'  \item{x2}{Chi-squared p-value}
#' }
#' @author John James
#' @export
verifyLinguistics <- function(fullCorpus, sampleCorpus,
                              chunks = 100, chunkSize = 2000) {

  startTime <- Sys.time()
  futile.logger::flog.info(paste("Entering function: verifyLinguistics",
                                 "Comparing lexical features for",
                                 "full and sample corpora"), name = 'green')

  # Select POS Tags of interest
  posTags <- subset(posTags, Study == TRUE)

  # Format corpora list
  korpora <- list()
  korpora$hc <- fullCorpus
  korpora$pc <- sampleCorpus

  # Conduct analyses
  analyses <- lapply(seq_along(korpora), function (c) {
    verifyCorpusLexicalFeatures(korpora[[c]], chunks, chunkSize)
  })

  # Calculate means of lexical feature counts
  aMeans <- mean(as.data.frame(analyses[[1]]$featureMatrix)[,1])
  bMeans <- mean(as.data.frame(analyses[[2]]$featureMatrix)[,1])

  for (i in 2:nrow(posTags)) {
    aMeans <- rbind(aMeans, mean(as.data.frame(analyses[[1]]$featureMatrix)[,i]))
    bMeans <- rbind(bMeans, mean(as.data.frame(analyses[[2]]$featureMatrix)[,i]))
  }


  # Create summary data table
  tagMeans <- data.table(Tag = posTags[,('Tag')],
                         Description = posTags[,('Description')],
                         'HC Corpus' = aMeans[,1], 'Training Corpus' = bMeans[,1])


  # Compute chi-squared statistic
  x2 <- chisq.test(aMeans, bMeans)

  # Summarize results
  features <- list(
    analyses = analyses,
    means = tagMeans,
    pValue = x2$p.value
  )

  # Create log entry
  endTime <- Sys.time()
  msg <- paste('Exiting function: verifyLinguistics.',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')

  return(features)

}
## ---- end

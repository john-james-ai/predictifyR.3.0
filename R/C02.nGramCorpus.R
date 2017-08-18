## ---- nGram_corpus

#==============================================================================#
#                               nGramCorpus                                    #
#==============================================================================#
#'  nGramCorpus
#'
#'  \code{nGramCorpus} Creates unigrams, bigrams, trigrams, and quadgrams
#'
#' This function takes a corpus meta data as an argument and  prepares unigrams,
#' bigrams, trigrams, and quadgrams for a corpus and stores it in the directory
#' designated in the corpus meta data.
#'
#' @param korpus - the meta data for pilot corpus
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
nGramCorpus <- function(korpus) {

  startTime <- Sys.time()
  futile.logger::flog.info(paste("Entering Function: nGramCorpus.",
                                 "Creating nGrams for", korpus$corpusName),
                           name = 'green')

  # Load corpus
  document <- unlist(lapply(seq_along(korpus$documents), function(d) {
    unlist(readFile(korpus$documents[[d]]))
  }))


  nGramCounts <- lapply(seq_along(korpus$nGrams), function(n) {
    futile.logger::flog.info(paste("...processing", korpus$nGrams[[n]]$fileDesc),
                             name = 'green')
    korpus$nGrams[[n]]$data <- quanteda::dfm(document, ngrams = n,
                                             remove_punct = FALSE,
                                             concatenator = ' ',
                                             tolower = FALSE)
    saveObject(korpus$nGrams[[n]])
    length(featnames(korpus$nGrams[[n]]$data))
  })

  nGrams <- list()
  nGrams$corpus <- korpus$corpusName
  nGrams$unigrams <- nGramCounts[[1]]
  nGrams$bigrams <- nGramCounts[[2]]
  nGrams$trigrams <- nGramCounts[[3]]
  nGrams$quadgrams <- nGramCounts[[4]]

  endTime <- Sys.time()
  futile.logger::flog.info(paste('Exiting Function: nGramCorpus. Elapsed time is',
                                 format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')

  return(nGrams)

}
## ---- end

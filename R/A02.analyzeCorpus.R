## ---- analysis_data
#==============================================================================#
#                                 getStats                                     #
#==============================================================================#
#'  getStats
#'
#'  \code{getStats} returns descriptive statistics for a document
#'
#' This function returns descriptive statistics for a document, including
#' object size, sentences, tokens, words, types, characters, and
#' word length statistics.
#'
#' @param document The document content to be analyzed
#' @return stats A list containing descriptive statistics
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family text processing functions
#' @export
getStats <- function(document) {

  # Tokenize document
  futile.logger::flog.info('......tokenize the document', name = 'green')
  tokens <- unlist(quanteda::tokenize(unlist(document$data), what = 'word'))

  # Extract Words
  futile.logger::flog.info("......extracting words", name = 'green')
  words <- grep(regexPatterns$words, tokens, value = TRUE, perl = TRUE)
  words <- words[nchar(words) > 0]

  futile.logger::flog.info("......extracting descriptive statistics", name = 'green')
  stats <- list()
  stats$category   <- document$fileDesc
  stats$objectSize <- as.numeric(round((object.size(tokens) / 1000000), 1))
  stats$sentences  <- length(document$data)
  stats$tokens     <- length(tokens)
  stats$words      <- length(words)
  stats$wordTypes  <- length(unique(words))
  stats$chars      <- sum(nchar(words))
  stats$wordsPerSent    <- stats$words / stats$sentences
  stats$minWordLength   <- min(nchar(words))
  stats$maxWordLength   <- max(nchar(words))
  stats$meanWordLength  <- mean(nchar(words))
  return(stats)

}

#==============================================================================#
#                             summarizeAnalysis                                #
#==============================================================================#
#'  summarizeAnalysis
#'
#'  \code{summarizeAnalysis} summarizes descriptive statistics
#'
#' This function takes as its parameters, the corpus documents and the
#' descriptive stats for the registers and produces summary statistics
#' at the corpus level.
#'
#' @param korpus The corpus documents
#' @param stats The descriptive stats for the three registers.
#' @return totals A data frame of descriptive statistics and totals
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family text processing functions
#' @export
summarizeAnalysis <- function(korpus, stats) {

  futile.logger::flog.info("...summarizing corpus feature totals", name = 'green')

  # Obtain word types across the combined corpus
  futile.logger::flog.info("......combine and tokenizing corpus", name = 'green')
  sents <- unlist(lapply(seq_along(korpus), function(d) {
    korpus[[d]]$data
  }))
  tokens <- unlist(quanteda::tokenize(sents, what = 'word'))

  # Extract Word Types
  words <- grep(regexPatterns$words, tokens, value = TRUE, perl = TRUE)
  words <- words[nchar(words) > 0]

  futile.logger::flog.info("......summarizing Descriptive Statistics", name = 'green')
  totals <- list()
  totals$category	  <- 	"Corpus"
  totals$objectSize <- sum(stats$objectSize)
  totals$sentences  <- 	sum(stats$sentences)
  totals$tokens	  <- 	sum(stats$tokens)
  totals$words	  <- 	sum(stats$words)
  totals$wordTypes	  <- 	length(unique(words))
  totals$chars 	  <- 	sum(stats$chars)
  totals$wordsPerSent    <- length(words) / sum(stats$sentences)
  totals$minWordLength	  <- min(nchar(words))
  totals$maxWordLength	  <- max(nchar(words))
  totals$meanWordLength	<- mean(nchar(words))

  totals <- rbind(stats, totals)

  return(totals)

}
#==============================================================================#
#                              analyzeCorpus                                     #
#==============================================================================#
#'  analyzeCorpus
#'
#'  \code{analyzeCorpus} summarizes descriptive statistics fora corpus
#'
#' This function takes as its parameters, the type of analysis (full or fast),
#' and the meta data for the corpus
#'
#' @param korpus - the meta data for the corpora to be analyzed
#' @return analysis - data frame of descriptive statistics
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family text processing functions
#' @export
analyzeCorpus <- function(korpus) {

  startTime <- Sys.time()

  futile.logger::flog.info(paste("Conducting a Analysis of", korpus$corpusName), name = 'green')

  documents <- lapply(seq_along(korpus$documents), function(d) {
    k <- list()
    k$directory <- korpus$documents[[d]]$directory
    k$fileName <- korpus$documents[[d]]$fileName
    k$fileDesc <- korpus$documents[[d]]$fileDesc
    k$data <- readFile(korpus$documents[[d]])
    k
  })

  # Inspect Individual Corpus Files
  stats <- data.table::rbindlist(lapply(seq_along(documents),function(x) {
    futile.logger::flog.info(paste("...analyzing", korpus$documents[[x]]$fileDesc), name = 'green')
    getStats(documents[[x]])
  }))

  # Get totals for corpus
  totals <- summarizeAnalysis(documents, stats)


  # Log
  endTime <- Sys.time()
  msg <- paste(korpus$corpusName, 'analysis complete. Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')

  return(totals)
}
## ---- end

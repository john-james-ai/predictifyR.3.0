## ---- tag_corpus

#==============================================================================#
#                                tagCorpus                                     #
#==============================================================================#
#'  tagCorpus
#'
#'  \code{buildPilot} POS tag a corpus
#'
#' This function takes as its parameter, the meta data for a corpus and performs
#' POS tagging of the corpus documents and stores the results in the
#' document /  POS directory
#'
#' @param korpus - meta data for the corpus to be tagged
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
tagCorpus <- function(korpus) {

  startTime <- Sys.time()
  futile.logger::flog.info(paste("POS tagging", korpus$corpusName), name = 'green')

  lapply(seq_along(korpus$documents), function(r) {
    korpus$documents[[r]]$data <- unlist(readFile(korpus$documents[[r]]))
    posData <- tagDocument(korpus$documents[[r]])

    # Saving POS Data
    korpus$pos$tags[[r]]$data <- unlist(posData$tags)
    korpus$pos$pairs[[r]]$data <- unlist(posData$pairs)
    saveFile(korpus$pos$tags[[r]])
    saveFile(korpus$pos$pairs[[r]])
  })


  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(
    paste('POS tagging of corpus complete. Elapsed time is',
          format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
    name = 'green')

}

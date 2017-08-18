## ---- build_corpora
#------------------------------------------------------------------------------#
#                              buildCorpora                                    #
#------------------------------------------------------------------------------#
#'  buildCorpora
#'
#'  \code{buildCorpora} Build training, validation and test sets
#'
#'
#' This function creates the training, validation, and test sets. Four training
#' sets would be comprised of two, four, eight,and sixteen pilot corpora. The
#' validation and test sets will be comprised of a single pilot corpus each,
#'
#' @param design Dataframe containing pilot and corpora design
#' @param fullCorpus List containing the meta data for the full corpus
#' @param training List containing  the meta data for the training corpora
#' @param validation List containing  the meta data for the validation corpus
#' @param test List containing  the meta data for the test corpus
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
buildCorpora <- function(design, fullCorpus, training,
                         validation, test) {

  startTime <- Sys.time()
  futile.logger::flog.info("Entering function: buildCorpora", name = 'green')

  #---------------------------------------------------------------------------#
  #                         Reading HC Corpus                                 #
  #---------------------------------------------------------------------------#

  # Read full corpus and break into chunks
  korpus <- lapply(seq_along(fullCorpus$documents), function(d) {
    document <- readFile(fullCorpus$documents[[d]])
    chunkDocument(document, design$pilot$`Sentences per Chunk`[d])
  })

  #---------------------------------------------------------------------------#
  #                 Build Training, Validation and Test Sets                  #
  #---------------------------------------------------------------------------#

  # Iterate through corpus registers
  lapply(seq_along(korpus), function(r) {
    futile.logger::flog.info(paste("...processing",
                                   fullCorpus$documents[[r]]$fileDesc),
                             name = 'green')

    futile.logger::flog.info("......creating validation set", name = 'green')
    pool <- 1:length(korpus[[r]])
    chunks <- design$corpusDesign$Validation[r] /
      design$pilot$`Sentences per Chunk`[r]
    set.seed(0505)
    valIndices <- sample(pool, chunks)
    validation$documents[[r]]$data <- unlist(korpus[[r]][valIndices])
    saveFile(validation$documents[[r]])

    futile.logger::flog.info("......creating test set", name = 'green')
    pool <- pool[!pool %in% valIndices]
    chunks <- design$corpusDesign$Test[r] /
      design$pilot$`Sentences per Chunk`[r]
    set.seed(0505)
    testIndices <- sample(pool, chunks)
    test$documents[[r]]$data <- unlist(korpus[[r]][testIndices])
    saveFile(test$documents[[r]])

    futile.logger::flog.info("......creating training sets", name = 'green')
    pool <- pool[!pool %in% testIndices]
    lapply(seq_along(training), function(t) {
      set.seed(0505)
      chunks <- design$corpusDesign[r, t+1] /
        design$pilot$`Sentences per Chunk`[r]
      trainingIndices <- sample(pool, chunks)
      training[[t]]$documents[[r]]$data <- unlist(korpus[[r]][trainingIndices])
      saveFile(training[[t]]$documents[[r]])
    })
  })

  # Create log entry
  endTime <- Sys.time()
  msg <- paste('Exiting function: buildCorpora.',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')
}
## ---- end

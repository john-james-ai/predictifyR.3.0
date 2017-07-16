## ---- verify_vocabulary

#==============================================================================#
#                         verifyVocabulary                                     #
#==============================================================================#
#'  verifyVocabulary
#'
#'  \code{verifyVocabulary} Verifies vocabulary coverage for a corpus
#'
#' This function takes as its parameter, the meta data for a full corpus and
#' for the sample corpus and checks the out of vocabulary (OOV) rates of
#' a sample corpus vis-a-vis a "full corpus".  Words in the full corpus not
#' found in the sample corpus are "OOV"
#'
#' @param fullCorpus - the meta data for the full corpus
#' @param sampleCorpus - the meta data for the sample corpus
#' @return oovRates - the OOV Rate for each corpus
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
verifyVocabulary <- function(fullCorpus, sampleCorpus) {

  startTime <- Sys.time()
  futile.logger::flog.info(paste("Entering function: verifyVocabulary",
                                 "Verifying", sampleCorpus$corpusName,
                                 "vocabulary"), name = 'green')

  coverage <- rbindlist(lapply(seq_along(fullCorpus$documents), function(d) {

    futile.logger::flog.info(paste('...obtaining full corpus',
                                   fullCorpus$documents[[d]]$fileDesc),
                             name = 'green')
    fullCorpusDoc <- unlist(readFile(fullCorpus$documents[[d]]))
    fullCorpusDocDfm <- quanteda::dfm(fullCorpusDoc)
    fullCorpusDocV  <- quanteda::nfeature(fullCorpusDocDfm)
    fullCorpusDocN  <- sum(ntoken(fullCorpusDocDfm))

    futile.logger::flog.info(paste('...obtaining sample corpus',
                                   sampleCorpus$documents[[d]]$fileDesc),
                             name = 'green')
    sampleDoc <- paste(unlist(readFile(sampleCorpus$documents[[d]])), collapse = ' ')
    sampleDoc <- unlist(tokenize(sampleDoc, what = 'word'))
    sampleDocDfm <- quanteda::dfm(sampleDoc)
    sampleDocV <- quanteda::nfeature(sampleDocDfm)
    sampleDocN <- sum(ntoken(sampleDocDfm))

    futile.logger::flog.info('...obtaining OOV Words', name = 'green')
    fullCorpusDoc <- quanteda::tokenize(fullCorpusDoc, what = 'word')
    oov <- quanteda::dfm(fullCorpusDoc, remove = sampleDoc, valuetype = 'fixed')
    Voov <- quanteda::nfeature(oov)
    Noov <- sum(ntoken(oov))

    futile.logger::flog.info('...calculating OOV rate and coverage',
                             name = 'green')
    oovRate  <- Noov / fullCorpusDocN * 100
    coverage <- 100 - oovRate

    # Format output
    oov <- list()
    oov$register <- registers[[d]]$fileDesc
    oov$fullCorpusV <- fullCorpusDocV
    oov$fullCorpusN <- fullCorpusDocN
    oov$docV <- sampleDocV
    oov$docN <- sampleDocN
    oov$Noov <- Noov
    oov$rate  <- oovRate
    oov$coverage <- coverage
    oov
  }))

  # Format total line and variable names
  coverage <- as.data.frame(coverage)

  fullCorpusV <- sum(coverage$fullCorpusV)
  fullCorpusN <- sum(coverage$fullCorpusN)
  docV <- sum(coverage$docV)
  docN <- sum(coverage$docN)
  Noov <- sum(coverage$Noov)
  rate  <- mean(coverage$rate)
  cover <- mean(coverage$coverage)
  ttl <- data.frame(register = 'Corpus', fullCorpusV = fullCorpusV, fullCorpusN = fullCorpusN,
                    docV = docV, docN = docN, Noov = Noov, rate = rate, coverage = cover,
                    stringsAsFactors = FALSE)
  coverage <- rbind(coverage, ttl)
  names(coverage) <- c('Registers', 'HC Corpus Vocabulary', 'HC Corpus Tokens',
                       'Pilot Corpus Vocabulary', 'Pilot Corpus Tokens',
                       '# OOV Words', 'OOV Rate', 'Coverage')

  # Create log entry
  endTime <- Sys.time()
  msg <- paste('Exiting function: verifyVocabulary. Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')
  return(coverage)
}
## ---- end

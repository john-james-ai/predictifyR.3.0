## ---- estimate_register_size
#------------------------------------------------------------------------------#
#                            estimateRegisterSize                              #
#------------------------------------------------------------------------------#
#'  estimateRegisterSize
#'
#'  \code{estimateRegisterSize} Estimates the size of individual registers
#'
#' This function takes as its parameters, the meta data for the korpus,
#' the corpus sample size estimate from \code{\link{estimateCorpusSize}},
#' the sampling unit estimate from \code{\link{estimateSamplingUnit}}, the
#' POS tags, sample size, and the number of samples and returns an estimate of
#' register size for each register based upon the distribution of lexical
#' features per 2000-word samples of the text. This analysis is based upon
#' Biber's 1993 Representativeness in Corpus Design.
#'
#' @param korpus List containing the meta data for the corpus
#' @param corpusSize List containing teh corpus size estimate from \code{\link{estimateCorpusSize}}
#' @param samplingUnit - List containing the sampling unit estimate from \code{\link{estimateSamplingUnit}}
#' @param sampleSize Integer indicating sampling unit size
#' @param numSamples Integer indicating the number of samples to analyze
#' @return registerSizes Dataframe containing
#'\itemize{
#' \item{Register}{String indicating the register name}
#' \item{Base}{Numeric indicating base allocation of samples allocated to all registers}
#' \item{Avg Vc}{Numberic average coefficient of variation across all POS tags}
#' \item{lambda}{Numeric factor multiplied by Avg Vc to calculate proportional allocation}
#' \item{Proportion}{Numeric indicating the proportional allocation of samples to registers}
#' \item{Num Samples}{Integer indicating Base + Proportion for each register}
#' \item{Sample Length}{Integer indicating length of each sample in tokens}
#' \item{Sample Size}{Integer = Num Samples * Sample Length}
#'}
#' @family sample size estimate functions
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
estimateRegisterSize <- function(korpus, corpusSize,
                                 samplingUnit, sampleSize = 2000,
                                 numSamples = 100) {

  startTime <- Sys.time()
  futile.logger::flog.info("Estimating register size",
                           name = 'green')

  # Designate POS tags used in the study
  posTags <- subset(posTags, Study == TRUE)

  # Conduct POS analysis on each register
  posAnalysis <- lapply(seq_along(korpus$documents), function(x) {
    futile.logger::flog.info(paste('...loading', korpus$documents[[x]]$fileDesc),
                             name = 'green')
    document <- readFile(korpus$documents[[x]])

    futile.logger::flog.info('...tokenizing document',
                             name = 'green')
    document <- unlist(quanteda::tokenize(document, what = 'word'))

    futile.logger::flog.info(paste0('...sampling ', numSamples, ', ',
                                    sampleSize, '-word samples'),
                             name = 'green')
    chunks <- sampleData(document, numChunks = numSamples,
                         chunkSize = sampleSize, format = 'lv')

    futile.logger::flog.info('...conducting POS analysis',
                             name = 'green')
    posAnalysis <- analyzeLexicalFeatures(chunks, posTags)
    posAnalysis
  })

  # Format parameters based upon corpus size estimate
  total <- corpusSize$n
  base  <- round(total * .1, 0)
  pool  <- total - (base * length(registers))
  sumAvgVc <-
    sum(unlist(lapply(seq_along(posAnalysis), function(x) {
      as.numeric(posAnalysis[[x]]$avgVc)
    })))
  lambda <- round(pool / sumAvgVc, digits = 0)

  registerSize <- rbindlist(lapply(seq_along(posAnalysis), function(x) {
    register <- registers[[x]]$fileDesc
    avgVc <- posAnalysis[[x]]$avgVc
    proportion <- round(avgVc * lambda, digits = 0)
    numSamples <- base + proportion
    sampleLength <- samplingUnit[[length(samplingUnit)]]$size
    numTokens <- numSamples * sampleLength
    size <- data.frame(register = register, base = base, avgVc = avgVc,
                       lambda = lambda, proportion = proportion,
                       numSamples = numSamples, sampleLength = sampleLength, sampleSize = numTokens)
    size
  }))

  # Add summary row to register table
  register = 'Corpus'
  base =  sum(registerSize$base)
  avgVc = sum(registerSize$avgVc)
  lambda = mean(as.numeric(registerSize$lambda))
  proportion = sum(registerSize$proportion)
  numSamples = sum(registerSize$numSamples)
  sampleLength = mean(as.numeric(registerSize$sampleLength))
  sampleSize = sum(registerSize$sampleSize)
  lastRow <- data.frame(register = register, base = base, avgVc = avgVc,
                        lambda = lambda, proportion = proportion,
                        numSamples = numSamples, sampleLength = sampleLength,
                        sampleSize = sampleSize)
  registerSize <- rbind(registerSize, lastRow)

  names(registerSize) <- c('Register', 'Base', 'Avg VC', 'Lambda', 'Proportion',
                           'Num Samples', 'Sample Length', 'Sample Size')

  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(
    paste('Register size estimates complete. Elapsed time is',
          format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')

  return(registerSize)
}
## ---- end

## ---- design_model_corpora
#------------------------------------------------------------------------------#
#                              designModelCorpora                              #
#------------------------------------------------------------------------------#
#'  designModelCorpora
#'
#'  \code{designModelCorpora} Designs the model corpora
#'
#' This function takes as its parameters, the the vocabulary based sample size
#' estimate from \code{\link{estimateSampleSize}}, the register size estimates
#' from \code{\link{estimateRegisterSize}}, the sampling unit estimate
#' from \code{\link{estimateSamplingUnit}}, and the clean data analysis from
#' \code{\link{analyzeData}}, and produces the model corpora design including
#' sizes for four training sets, a validation set and a test set.
#'
#' @param sampleSizeEstimate - the vocabulary-based estimate
#' @param registerSizeEstimate - the lexical feature based estimate
#' @param samplingUnit - the sampling unit
#' @param analysis - the clean data analysis
#' @return alpha - the corpus design for alpha training set.
#' @author John James
#' @export
designModelCorpora <- function(sampleSizeEstimate, registerSizeEstimate,
                        samplingUnit, analysis) {

  startTime <- Sys.time()
  futile.logger::flog.info("Designing model corpora",
                           name = 'green')


  #---------------------------------------------------------------------------#
  #                  Compute Estimated Pilot Corpus Size                      #
  #---------------------------------------------------------------------------#
  register <- c(as.character(registerSizeEstimate$Register[1:3]), 'Corpus')
  hcCorpus <- analysis$tokens
  diversity <- as.vector(t(sampleSizeEstimate$sampleSize[1:4,3]))
  syntactic <- round(registerSizeEstimate$'Sample Size'[1:4], 0)
  tokens <- pmax(diversity, syntactic, na.rm = TRUE)
  sentences <- floor(tokens[1:3] / analysis$wordsPerSent[1:3])
  sentences <- c(sentences, sum(sentences))
  pctTotal <- round(tokens / analysis$tokens[1:4] * 100, 0)
  proportion <- round(tokens[1:3] / sum(tokens[1:3]) * 100, 0)
  proportion <- c(proportion, 100)

  comparison <- data.frame(register = register, hcCorpus = hcCorpus,
                           diversity = diversity,
                           syntactic = syntactic, tokens = tokens,
                           sentences = sentences, pctTotal = pctTotal,
                           proportion = proportion)

  names(comparison) <- c('Register', 'HC Corpus', 'Diversity-Based Estimate',
                         'Lexical Feature-Based Estimate',
                         'Tokens', 'Sentences', '% Total',
                         'Proportion')
  #---------------------------------------------------------------------------#
  #                  Compute Extrapolated Pilot Corpus Size                   #
  #---------------------------------------------------------------------------#
  register <- c(as.character(registerSizeEstimate$Register[1:3]), 'Corpus')
  hcTokens <- analysis$tokens
  extrapolated <- pctTotal / pctTotal[4] * 5
  extrapolatedTokens <- hcTokens * extrapolated / 100
  extrapolatedSents <- floor(extrapolatedTokens / analysis$wordsPerSent)
  chunkSize <- rep(samplingUnit[[length(samplingUnit)]]$size, 4)
  sentsPerChunk <- floor(samplingUnit[[length(samplingUnit)]]$size /
                           analysis$wordsPerSent)
  chunks <- floor(extrapolatedSents / sentsPerChunk)
  sampleSize <- chunks * sentsPerChunk

  pilot <- data.frame(register = register, hcTokens = hcTokens,
                      extrapolated = extrapolated,
                      extrapolatedTokens = extrapolatedTokens,
                      extrapolatedSents = extrapolatedSents,
                      chunkSize = chunkSize,
                      sentsPerChunk = sentsPerChunk,
                      chunks = chunks,
                      sampleSize = sampleSize)
  names(pilot) <- c('Register', 'HC Corpus (Tokens)',
                    '% Total', 'Tokens', 'Sentences',
                    'Chunk Size (Tokens)',
                    'Sentences per Chunk', '# Chunks',
                    'Sample Size (Sentences)')

  #---------------------------------------------------------------------------#
  #           Compute Model Corpora (Train, Validation, Test) Sizes           #
  #---------------------------------------------------------------------------#
  # Compute training, validation and test set sizes
  Registers <- c('Blogs', 'News', 'Twitter', 'Corpus')
  Validation <- pilot$`Sample Size (Sentences)`[1:3]
  Test <- pilot$`Sample Size (Sentences)`[1:3]
  Alpha <-  pilot$`Sample Size (Sentences)`[1:3] * 2
  Beta <-  pilot$`Sample Size (Sentences)`[1:3] * c(5,2,5)
  Gamma <-  pilot$`Sample Size (Sentences)`[1:3] * c(10,2,10)
  Delta <-  pilot$`Sample Size (Sentences)`[1:3] * c(14,2,14)
  corpusDesign <- data.frame(Alpha, Beta, Gamma, Delta, Validation, Test)
  ttl <- colSums(corpusDesign)
  corpusDesign <- rbind(corpusDesign, ttl)
  corpusDesign <- as.data.frame(cbind(Registers, corpusDesign), stringsAsFactors=FALSE)

  # Format results
  design <- list(
    comparison = comparison,
    pilot = pilot,
    corpusDesign = corpusDesign
  )

  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(paste('Model corpora design complete. Elapsed time is',
                                 format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')


  return(design)
}
## ---- end

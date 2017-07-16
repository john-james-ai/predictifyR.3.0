#------------------------------------------------------------------------------#
#                                 estimateSampleSize                           #
#------------------------------------------------------------------------------#
#'  estimateSampleSize
#'
#'  \code{estimateSampleSize} Estimates sample sizes for various OOV rates
#'
#' This function takes as its parameters, the corpus meta data and the analysis
#' of the clean corpus then uses LNRE models from the zipfR package to
#' calculate and evaluate various samples sizes and associated OOV Rates.
#'
#' @param korpus - the meta data for the HC Corpus
#' @param analysis - the analysis of the HC Corpus
#' @return estimate - list of data frames containing coverage estimates
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @seealso
#'   \code{\link[languageR]{text2spc.fnc}}
#'   \code{\link[zipfR]{lnre}}
#'   \code{\link[zipfR]{lnre.spc}}
#'   \code{\link[zipfR]{N}}
#'   \code{\link[zipfR]{V}}
#'   \code{\link[zipfR]{EV}}
#' @family sample size estimate functions
#' @export
estimateSampleSize <- function(korpus, analysis) {

  startTime <- Sys.time()
  futile.logger::flog.info("Estimating vocabulary-based sample size",
                           name = 'green')

  # Designate sample sizes in terms of percent HC Corpora registers
  sampleSizes <- c(.1,.5,1,3,5,10,15)

  # Iterate through registers, samples sizes, and coverages
  coverage <- data.table::rbindlist(lapply(seq_along(korpus$documents), function(d) {
    futile.logger::flog.info(paste("...processing",korpus$documents[[d]]$fileDesc),
                             name = 'green')

    document <- readFile(korpus$documents[[d]])
    tokens <- unlist(quanteda::tokenize(document, what = 'word'))

    # Create spectrum objects.  A frequency spectrum lists the number of unique
    # that occur once, twice, thrice, and so on.
    hcDocSpc  <- languageR::text2spc.fnc(tokens)
    hcDocSpc$m  <- as.integer(hcDocSpc$m)
    hcDocSpc$Vm <- as.integer(hcDocSpc$Vm)
    hcDocV <- zipfR::V(hcDocSpc)
    hcDocN <- zipfR::N(hcDocSpc)
    hcDocSpc$Voov <- cumsum(hcDocSpc$Vm)
    hcDocSpc$Noov <- cumsum(hcDocSpc$Vm * hcDocSpc$m)

    # Iterate through sample sizes
    registerAnalysis <- data.table::rbindlist(lapply(seq_along(sampleSizes), function(s) {
      futile.logger::flog.info(paste("......processing",sampleSizes[s], '% of document'),
                               name = 'green')

      # Sample the document according to the sampleSizes variable
      numSamples <- floor(hcDocN * sampleSizes[s] / 100)
      sampleSize <- 1
      sampleDoc <- sampleData(tokens, numChunks = numSamples, chunkSize = sampleSize, format = 'v')

      # Create sample document observed spectrum object.
      sampleDocSpc    <- languageR::text2spc.fnc(sampleDoc)
      sampleDocSpc$m  <- as.integer(sampleDocSpc$m)
      sampleDocSpc$Vm <- as.integer(sampleDocSpc$Vm)

      # Obtain vocabulary size for sample data
      vSample <- zipfR::V(sampleDocSpc)

      # Train ZM model on sample data.
      docLnre <- zipfR::lnre('zm', spc = sampleDocSpc, cost = 'chisq',
                             method = 'Custom', exact  = FALSE)

      # Extrapolate spectrum for zm model out to the size N of the
      # original document
      extSpc <- zipfR::lnre.spc(docLnre, hcDocN, m.max = nrow(hcDocSpc))
      extSpc$m  <- as.integer(extSpc$m)
      extSpc$Vm <- as.integer(extSpc$Vm)
      extSpc$Voov <- cumsum(extSpc$Vm)
      extSpc$Noov <- cumsum(extSpc$Vm * extSpc$m)

      # Return the expected vocabulary size at N
      vExt <- zipfR::EV(docLnre, hcDocN)

      # Calculate estimated Voov vis-a-vis vocabulary calculated
      # from observed spectrum.
      eVoov <- vExt - vSample

      # Calculate estimated Noov
      eNoov <- max(0,head(subset(extSpc, extSpc$Voov > eVoov, select = Noov)$Noov, 1))

      # Calculate OOV Rate in percent
      oovRate <- eNoov / hcDocN * 100

      # Calculate Coverage
      coverage <- 100 - oovRate

      # Format output
      result <- list()
      result$register <- korpus$documents[[d]]$fileDesc
      result$percent  <- sampleSizes[s]
      result$size     <- numSamples
      result$vSample <- vSample
      result$vExt <- vExt
      result$eVoov <- eVoov
      result$eNoov <- eNoov
      result$oovRate <- oovRate
      result$coverage <- coverage
      result
    }))
    registerAnalysis
  }))



  # Summarize estimates at approximately 95% coverage
  ss <- subset(coverage, coverage > 96 & percent > 1)
  ss <- data.table(ss)[, .SD[which.min(coverage)], by = register]
  ss <- data.frame(ss)

  # Format Summary Row
  register <- 'Corpus'
  percent <- sum(ss$size) / analysis$tokens[4] * 100
  size <- sum(ss$size)
  vSample <- sum(ss$vSample)
  vExt <- sum(ss$vExt)
  eVoov <- sum(ss$eVoov)
  eNoov <- sum(ss$eNoov)
  oovRate <- mean(ss$oovRate)
  cover <- mean(ss$coverage)
  ttl <- data.frame(register = register, percent = percent, size = size,
                    vSample = vSample, vExt = vExt, eVoov = eVoov,
                    eNoov = eNoov, oovRate = oovRate, coverage = cover,
                    stringsAsFactors = FALSE)
  ss <- rbind(ss, ttl)


  # Format Results
  estimates <- list()
  estimates$coverage <- coverage
  estimates$sampleSize <- ss

  # Closing Log
  endTime <- Sys.time()
  futile.logger::flog.info(paste('Sample size estimate complete. Elapsed time is',
                                 format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')

  return(estimates)
}
## ---- end

## ---- analyze_sampling_unit
#------------------------------------------------------------------------------#
#                            estimateSamplingUnit                              #
#------------------------------------------------------------------------------#
#'  estimateSamplingUnit
#'
#'  \code{estimateSamplingUnit} Estimates the sampling unit for corpus sampling
#'
#' This function takes as its parameters, the korpus meta data and the POS tags
#' selected for this analysis and compares the distributions of lexical features
#' across pairs of samples of varying sizes. The results of chi-squared tests
#' for selected features are averaged over the samples.  The function returns
#' a data frame indicating average chi-squared p-values for each feature and
#' sampling unit size.
#'
#' @param korpus List containing the meta data for the corpus
#' @param sampleSizes Integer vector of sample sizes to be evaluated
#' @param numSamples Integer indicating number of samples to evaluate
#' @return analysis A list containing:
#' \itemize{
#'  \item{sampleSize}{Sample size being tested}
#'  \item{scores(long)}{Long dataframe of chi-squared scores at various sample sizes}
#'  \item{scores(wide)}{Wide dataframe of chi-squared scores at various sample sizes}
#' }
#' @seealso
#'   \code{\link{analyzeLexicalFeatures}}
#'   \code{\link[languageR]{text2spc.fnc}}
#'   \code{\link[zipfR]{lnre}}
#'   \code{\link[zipfR]{lnre.spc}}
#'   \code{\link[zipfR]{N}}
#'   \code{\link[zipfR]{V}}
#'   \code{\link[zipfR]{EV}}
#'   \code{\link[stats]{chisq.test}}
#' @family sample size estimate functions
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
estimateSamplingUnit <- function(korpus, sampleSizes = c(100, 500, 1000, 2000),
                                 numSamples = 30) {

  startTime <- Sys.time()
  futile.logger::flog.info(paste("Entering Function: estimateSamplingUnit.",
                                 "Estimating sampling unit size"),
                           name = 'green')

  # Designate POS Tags to include in study
  posTags <- subset(posTags, Study == TRUE)

  analysis <- lapply(seq_along(sampleSizes), function(s) {
    futile.logger::flog.info(paste('...processing', sampleSizes[s],
                                   'token samples'),
                             name = 'green')

    # Process corpus documents
    scores <- data.table::rbindlist(lapply(seq_along(korpus$documents), function(x) {
      futile.logger::flog.info(paste('......processing',
                                     korpus$documents[[x]]$fileDesc),
                               name = 'green')
      document <- readFile(korpus$documents[[x]])
      tokens <- unlist(quanteda::tokenize(document, what = "word"))
      midway <- floor(length(tokens)/2)

      # Process Sample A
      sampleA <- sampleData(tokens[1:midway], numChunks = numSamples,
                            chunkSize = sampleSizes[s], format = 'lv')
      distA <- data.table(analyzeLexicalFeatures(sampleA)$featureMatrix)
      distA <- as.data.table(t(distA), keep.rownames = TRUE)
      setnames(distA, 'rn', 'Tag')

      # Process Sample B
      sampleB <- sampleData(tokens[(midway+1):length(tokens)],
                            numChunks = numSamples,
                            chunkSize = sampleSizes[s], format = 'lv')
      distB <- data.table(analyzeLexicalFeatures(sampleB)$featureMatrix)
      distB <- as.data.table(t(distB), keep.rownames = TRUE)
      setnames(distB, 'rn', 'Tag')

      # Create POS Table
      set.seed(230)
      posDt <- data.table(Tag = posTags$Tag)

      # Set Keys
      setkey(posDt, Tag)
      setkey(distA, Tag)
      setkey(distB, Tag)
      distA <- merge(posDt, distA, by='Tag', all.x = TRUE)
      distB <- merge(posDt, distB, by='Tag', all.x = TRUE)
      distA[is.na(distA)] <- 0
      distB[is.na(distB)] <- 0

      # Iterate through rows and run Chi-Sq Tests on distributions
      x2 <- data.table::rbindlist(lapply(seq_along(1:nrow(distA)), function(d) {
        a <- as.vector(t(distA[d,-1]))
        b <- as.vector(t(distB[d,-1]))
        dist <- data.frame(a = a, b = b)

        x2 <- list()
        x2$Size <- sampleSizes[s]
        x2$Register <- registers[[x]]$fileDesc
        x2$Pos      <- as.character(distA[d][,Tag])
        x2$Score <- stats::chisq.test(dist)$p.value
        x2
      }))
      x2
    }))
    analysis <- list()
    analysis$size <- sampleSizes[s]
    analysis$long <- scores
    analysis$wide <- reshape2::dcast(scores, Register + Size~Pos, value.var = "Score")

    # Create useful summary information
    # Total row with column means
    cm1 <- as.data.frame(t(colMeans(analysis$wide[,3:ncol(analysis$wide)],
                                    na.rm = TRUE)))
    cm2 <- data.frame(Register = 'Corpus Mean', Size = sampleSizes[s])
    cm1 <- cbind(cm2, cm1)
    analysis$wide <- rbind(analysis$wide, cm1)

    # Total column with row means
    rm <- data.frame(Mean = rowMeans(analysis$wide[,3:ncol(analysis$wide)],
                                     na.rm = TRUE))
    analysis$wide <- cbind(analysis$wide, rm)

    # Tags varying above alpha = .05 showing too much variation (tmv) for sample size
    analysis$tmv <- merge(as.data.table(subset(analysis$long, Score < 0.05 &
                                                 Size == sampleSizes[s])),
                          as.data.table(subset(posTags,
                                               select = c(Tag, Description))),
                          by.x = 'Pos', by.y = 'Tag')[,.(Description, Score, Register)]

    # Tags that vary the most
    analysis$mvt <- merge(as.data.table(subset(analysis$long, Size == sampleSizes[s] &
                                                 Score == min(subset(analysis$long,
                                                                     select = Score)))),
                          as.data.table(subset(posTags, select = c(Tag, Description))),
                          by.x = 'Pos', by.y = 'Tag')[,.(Description, Score, Register)]
    # Mean of all tags
    analysis$mean <- mean(as.numeric(analysis$wide[nrow(analysis$wide),
                                                   3:ncol(analysis$wide)]),
                          na.rm = TRUE)
    analysis
  })

  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(paste('Exiting Function: estimateSamplingUnit",
                                 "Elapsed time is',
                                 format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
                           name = 'green')

  return(analysis)
}
## ---- end

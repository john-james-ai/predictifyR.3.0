## ---- analyze_lexical_features

#==============================================================================#
#                           analyzeLexicalFeatures                             #
#==============================================================================#
#'  analyzeLexicalFeatures
#'
#'  \code{analyzeLexicalFeatures} Analyzes lexical features for a document
#'
#' This function takes as its parameters, the listed tokenized document
#' to be tagged and returns an analysis of lexical features for the document.
#'
#' @param document - the document to tag in unlisted token format
#' @return analysis - analysis of lexical density for the file
#'  \itemize{
#'   \item{avgVc}{Numeric average coefficient of variation across all POS tags}
#'   \item{chunkMatrix}{Data frame of POS tags for each document chunk}
#'   \item{featureMatrix}{Data frame of POS tag features for each document chunk}
#'   \item{featureStats}{Dataframe of descriptive statistics for each POS tag}
#'   \item{tagPairs}{Dataframe of token POS tag pairs}
#'
#'  }
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
analyzeLexicalFeatures <- function(document) {

  # Initialize Annotators
  sentAnnotator <- openNLP::Maxent_Sent_Token_Annotator()
  wordAnnotator <- openNLP::Maxent_Word_Token_Annotator()
  posAnnotator <- openNLP::Maxent_POS_Tag_Annotator()

  # Prepare a list of data frames, one per chunk, with feature frequencies
  taggedChunks <- lapply(seq_along(document), function(x) {
    #    cat("\r......tagging chunk", x, "out of", length(document), "chunks                 ")
    tagChunk(document[[x]], x, sentAnnotator, wordAnnotator,
             posAnnotator)
  })

  # Combine the list of long data frames into a single data frame for all chunks
  chunkMatrix <- data.table::rbindlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$tagsTableLong
  }))
  chunkMatrix[is.na(chunkMatrix)] <- 0

  # Combine the list of wide data frames into a single data frame for all chunks
  featureMatrix <- data.table::rbindlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$tagsTableWide
  }), fill = TRUE)
  featureMatrix[is.na(featureMatrix)] <- 0

  # Combine the list of data frames into a single data frame for all chunks
  tagPairs <- unlist(lapply(seq_along(taggedChunks), function(x) {
    taggedChunks[[x]]$pairs
  }))

  # Calculate descriptive statistics and sample sizes
  features <- names(featureMatrix)
  featureStats <- data.table::rbindlist(lapply(seq_along(featureMatrix), function(x) {
    min <- min(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    max <- max(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    mean <- mean(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    range <- max - min
    total <- sum(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    sd <- sd(as.numeric(as.character(featureMatrix[[x]])),na.rm=TRUE)
    vc <- sd / mean  # normalized deviation
    te <- .05 * mean
    n <- sd^2 / (te/1.96)^2
    tag <- features[[x]]
    desc <- subset(posTags, Tag == tag, select = Description)
    df <- data.frame(tag = tag, desc = desc, min = min, max = max,
                     mean = mean, range = range, total = total, sd = sd,
                     vc = vc, te = te, n = n)
    df[complete.cases(df),]
  }))
  total <- sum(featureStats$total)
  featureStats$pctTotal <- data.frame(pctTotal = featureStats$total
                                      / total * 100)
  featureStats <- featureStats[,c(1,2,3,4,5,6,7,12,8,9,10,11)]
  avgVc <- mean(featureStats$vc)

  analysis <- list(
    avgVc = avgVc,
    chunkMatrix = chunkMatrix,
    featureMatrix = featureMatrix,
    featureStats = featureStats,
    tagPairs = tagPairs
  )

  return(analysis)
}

## ---- end

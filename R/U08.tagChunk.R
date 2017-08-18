#==============================================================================#
#                                tagChunk                                      #
#==============================================================================#
#'  tagChunk
#'
#'  \code{tagChunk} POS tags chunk of text and returns POS counts for the chunk
#'
#' This function takes as its parameter, the a list of text to be tagged,
#' the chunk number, the POS tags and the sentence, word, and POS annotators
#' from the openNLP package and returns a one-row data frame containing
#' frequences of POS tags for the chunk.
#'
#' @param chunk - a chunk (list) of text to be tagged
#' @param chunkNum - the chunk number
#' @param sentAnnotator - the openNLP sentence annotator
#' @param wordAnnotator - the openNLP word annotator
#' @param posAnnotator - the openNLP POS annotator
#' @return tags - 1 row data frame with frequencies for each feature
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family POS Tagging Functions
#' @export
tagChunk <- function(chunk, chunkNum, sentAnnotator, wordAnnotator,
                     posAnnotator) {

  textData <- NLP::as.String(chunk)

  # Format sentence and word annotator
  a2 <- NLP::annotate(textData, list(sentAnnotator, wordAnnotator))

  # Format POS Annotator
  a3 <- NLP::annotate(textData, posAnnotator, a2)

  # Extract POS tag distributions
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tagsTable <- as.data.frame(table(tags))
  rownames(tagsTable) <- tagsTable[, 1]
  tagsTable$Freq <- as.numeric(tagsTable$Freq)
  tagsTable <- subset(tagsTable, tags %in% posTags$Tag)
  tagsTableLong <- cbind(chunkNum = chunkNum, tagsTable)
  tagsTableWide <- as.data.frame(t(tagsTable))
  tagsTableWide <- tagsTableWide[-1, ]

  # Convert factors to numeric and NAs to zeros
  tagsTableLong$Freq <- as.numeric(as.character(tagsTableLong$Freq))
  tagsTableLong$Freq[is.na(tagsTableLong$Freq)] <- 0

  tagsTableWide[] <- lapply(tagsTableWide, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  tagsTableWide[is.na(tagsTableWide)] <- 0

  # Extract word/pos tag pairs
  pairs <- sprintf("%s/%s", textData[a3w], tags)

  tagData <- list(tagsTableLong = tagsTableLong,
                  tagsTableWide = tagsTableWide,
                  pairs = pairs)

  return(tagData)
}

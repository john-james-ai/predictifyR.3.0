## ---- clean_data

#==============================================================================#
#                              cleanDocument                                   #
#==============================================================================#
#'  cleanDocument
#'
#'  \code{cleanDocument} cleans the HC Corpus document
#'
#' This function reads a corpus document and performs a series of conditioning,
#' reshaping, normalization, and clean up tasks.
#' The conditioning tasks include:
#' \itemize{
#'  \item{Control}{Replace UTF-8 control characters with spaces}
#'  \item{Quotations}{Convert non-standard quotations to single quote}
#'  \item{Encoding}{Convert document from UTF-8 to ASCII enconding}
#'  }
#'
#'  The reshaping task uses the quanteda package
#'  \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf} to
#'  reshape the corpus documents into sentences
#'
#'  The normalization tasks include the following:
#'  \itemize{
#'   \item{control}{Remove ASCII control characters}
#'   \item{nonprint}{Remove ASCII non-printable characters}
#'   \item{emails}{Remove email addresses}
#'   \item{urls}{Remove urls}
#'   \item{hashtags}{Remove twitter hashtags}
#'   \item{control}{Remove ASCII control characters}
#'   \item{digits}{Remove digits}
#'   \item{punct}{Remove punctuation except the apostrophe}
#'   \item{longWords}{Remove words 40 characters or longer}
#'   \item{profanity}{Remove profanity}
#'   \item{correct}{Correct contractions and common misspellings}#'
#'  }
#'
#'  Finally, the clean up tasks include:
#'  \itemize{
#'   \item{whiteSpace}{Remove extra whitespace from documents}
#'   \item{punct}{Remove stray apostrophes and punctuation}
#'   \item{emptySent}{Remove empty sentences}
#'  }
#'
#' @param rawDocument - the meta data and content for the file to be analyzed
#' @return cleanDocument Cleaned text document in unlisted vector format.
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family text processing functions
#' @export
cleanDocument <- function(rawDocument) {

  # Condition data - Remove null and substitute control characters
  futile.logger::flog.info(paste('......conditioning',rawDocument$fileDesc),
                           name = 'green')
  document <- readBin(file.path(rawDocument$directory, rawDocument$fileName),
                      raw(), file.info(file.path(rawDocument$directory,
                                                 rawDocument$fileName))$size)
  document[document == as.raw(0)] = as.raw(0x20)
  document[document == as.raw(26)] = as.raw(0x20)
  d <- tempfile(fileext = '.txt')
  writeBin(document, d)
  document <- readLines(d)
  unlink(d)

  # Condition data - Convert UTF-8 Quotations and hyphens and ASCII encode
  futile.logger::flog.info('......normalizing UTF-8 quotations and hyphens', name = 'green')
  Encoding(document) <- "latin1"
  document <- enc2utf8(document)
  document <- gsub("â€™", "'", document)
  document <- gsub("â€˜", "'", document)
  document <- gsub("â€¦", "", document)
  document <- gsub("â€", "-", document)
  document <- iconv(document, "UTF-8", "ASCII", sub = "")

  # Reshape into sentences and lower case
  futile.logger::flog.info('......reshaping document into sentences', name = 'green')
  document <- unlist(parallelizeTask(quanteda::tokenize, document, what = 'sentence'))
  document <- tolower(document)

  # Remove sentences containing profanity
  futile.logger::flog.info('......removing profanity', name = 'green')
  document <- predictifyR:::extractLines(document, badWords)

  # Perform corrections
  futile.logger::flog.info('......performing corrections', name = 'green')
  key <- paste0("\\b", corrections$key, "\\b")
  for (i in 1:length(key)) {
    document <- gsub(key[i], corrections$value[i], document, ignore.case = TRUE)
  }
  document <- gsub("\\&", 'and', document)
  # Perform normalizations
  for (i in 1:length(normalizer)) {
    futile.logger::flog.info(paste('......normalizing',normalizer[[i]]$desc),
                             name = 'green')
    document <- gsub(normalizer[[i]]$pattern, normalizer[[i]]$replace,
                     document, perl = TRUE)
  }

  # Remove extra whitespace and empty sentences
  futile.logger::flog.info('......cleaning up extra whitespace and empty sentences', name = 'green')
  document <- stringr::str_replace(gsub(regexPatterns$whiteSpace, " ",
                               stringr::str_trim(document)), "B", "b")
  document <- document[document != ""]
  document <- document[document != "'"]

  return(document)
}


#==============================================================================#
#                              cleanCorpus                                     #
#==============================================================================#
#'  cleanCorpus
#'
#'  \code{cleanCorpus} cleans the HC Corpus
#'
#' This function takes as its parameters, the meta data for the raw and clean
#' corpora, then performs normalization and cleaning tasks and stores it in
#' the directory designated in the meta data.
#'
#' @param raw - the meta data for the reshaped raw corpus
#' @param clean - the meta data for the clean corpus
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family text processing functions
#' @export
cleanCorpus <- function(raw, clean) {

  startTime <- Sys.time()
  futile.logger::flog.info("Commencing corpus cleaning", name = 'green')

  # Clean Document
  lapply(seq_along(raw$documents), function(x) {
    futile.logger::flog.info(paste('...cleaning', raw$documents[[x]]$fileDesc),
                             name = 'green')
    clean$documents[[x]]$data <- cleanDocument(raw$documents[[x]])
    saveFile(clean$documents[[x]])
  })

  # Log
  endTime <- Sys.time()
  msg <- paste('HC Corpora cleaned',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')
}
## ---- end

## ---- clean_data

#==============================================================================#
#                              cleanDocument                                   #
#==============================================================================#
#'  cleanDocument
#'
#'  \code{cleanDocument} cleans the HC Corpus document
#'
#' This function takes as its parameters, the meta data for the raw corpus
#' document and performs a series of normalization and data cleaning tasks
#' and returns the cleaned document to the calling environment.
#'
#' @param rawDocument - the meta data and content for the file to be analyzed
#' @return cleanDocument Cleaned text document in unlisted vector format.
#' @author John James
#' @export
cleanDocument <- function(rawDocument) {

  # Normalize data - Remove null and substitute control characters
  flog.info('......extracting null and substitute control characters', name = 'green')
  document <- readBin(file.path(rawDocument$directory, rawDocument$fileName),
                      raw(), file.info(file.path(rawDocument$directory,
                                                 rawDocument$fileName))$size)
  document[document == as.raw(0)] = as.raw(0x20)
  document[document == as.raw(26)] = as.raw(0x20)
  d <- tempfile(fileext = '.txt')
  writeBin(document, d)

  # Reshape data and convert to lower case
  flog.info('......reshaping document into sentences', name = 'green')
  document <- readLines(d)
  korpus <- parallelizeTask(quanteda::corpus, document)
  document <- parallelizeTask(quanteda::tokenize, korpus, what = 'sentence')
  document <- tolower(document)

  # Normalize - UTF-8 Quotations and hyphens
  flog.info('......normalizing UTF-8 quotations and hyphens', name = 'green')
  Encoding(document) <- "latin1"
  document <- enc2utf8(document)
  document <- gsub("â€™", "'", document)
  document <- gsub("â€˜", "'", document)
  document <- gsub("â€¦", "", document)
  document <- gsub("â€", "-", document)
  document <- iconv(document, "UTF-8", "ASCII", sub = "")

  # Conduct corrections
  flog.info('......performing corrections', name = 'green')
  key <- paste("\\b", corrections$key, "\\b", sep = "")
  value <- corrections$value
  for (i in 1:length(key)) {
    document <- gsub(key[i], value[i], document, perl = TRUE)
  }

  flog.info('......removing profanity', name = 'green')
  key <- paste("\\b", badWords$key, "\\b", sep = "")
  for (i in 1:length(key)) {
    document <- gsub(key[i], ' ', document, perl = TRUE)
  }

  normalize <- list(
    nonAscii = list(
      pattern = regexPatterns$nonAscii,
      desc    = 'non-Ascii characters',
      replace = ' '),
    nonPrintable = list(
      pattern = regexPatterns$nonPrintable,
      desc    = 'non-printable characters',
      replace = ' '),
    control = list(
      pattern = regexPatterns$control,
      desc    = 'control characters',
      replace = ' '),
    emails = list(
      pattern = regexPatterns$emails,
      desc    = 'email addresses',
      replace = ' '),
    urls = list(
      pattern = regexPatterns$urls,
      desc    = 'urls',
      replace = ' '),
    twitter = list(
      pattern = regexPatterns$twitter,
      desc    = 'twitter hashtags',
      replace = ' '),
    digits = list(
      pattern = regexPatterns$digits,
      desc    = 'digits',
      replace = ' '),
    punct = list(
      pattern = regexPatterns$punctSansApos,
      desc    = 'punctuation (except apostrophe)',
      replace = ' '),
    strays = list(
      pattern = regexPatterns$strayApostrophe,
      desc    = 'stray apostrophes',
      replace = ''),
    repeats = list(
      pattern = regexPatterns$repeatedChars,
      desc    = 'repeated character patterns',
      replace = '\\1'),
    longWords = list(
      pattern = regexPatterns$longWords,
      desc    = 'words > 40 characters',
      replace = '')

  )

  for (i in 1:length(normalize)) {
    flog.info(paste('......normalizing', normalize[[i]]$desc), name = 'green')
    document <- gsub(normalize[[i]]$pattern,
                        normalize[[i]]$replace,
                        document, perl = TRUE)
  }

  # Remove extra whitespace
  flog.info('......removing extra whitespace', name = 'green')
  document <- stringr::str_replace(gsub(regexPatterns$whiteSpace, " ",
                               stringr::str_trim(document)), "B", "b")

  flog.info("......removing empty sentences and sentences with just punctuation", name = 'green')
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
#' This function cleans the raw corpus data and stores it in the designated
#' directory
#'
#' @param raw - the meta data for the reshaped raw corpus
#' @param clean - the meta data for the clean corpus
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
cleanCorpus <- function(raw, clean) {

  startTime <- Sys.time()
  flog.info("Commencing corpus cleaning", name = 'green')

  # Clean Document
  lapply(seq_along(raw$documents), function(x) {
    flog.info(paste('...cleaning', raw$documents[[x]]$fileDesc), name = 'green')
    clean$documents[[x]]$data <- cleanDocument(raw$documents[[x]])
    saveFile(clean$documents[[x]])
  })

  # Log
  endTime <- Sys.time()
  msg <- paste('HC Corpora cleaned',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  flog.info(msg, name = 'green')
}
## ---- end

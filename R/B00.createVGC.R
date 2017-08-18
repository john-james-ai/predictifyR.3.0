## ---- create_vgc
#==============================================================================#
#                           createGrowthObject                                 #
#==============================================================================#
#'  createGrowthObject
#'
#'  \code{createGrowthObject} Creates vocabulary growth object for document
#'
#' This function takes as its parameter, the meta data for the document to be
#' analyzed, a string indicating the type of analysis to perform c('full', 'fast),
#' and the number of samples and returns  and returns vocabulary growth data
#' at increments designated by "samples". For full analysis, the vgc function
#' from the languageR package is used
#'
#' @param document The meta data for the document being analyzed
#' @param type - c('fast', 'full')
#' @param samples - the number of rows in the growth document
#' @return vgc - vocabulary growth curve data
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family vocabulary growth functions
#' @seealso
#'  \code{\link{createVGC}}
#'  \code{\link{createZipf}}
#'  \code{\link{createZipfObject}}
#'  \code{\link[quanteda]{tokenize}}
#'  \code{\link[languageR]{growth.fnc}}
#' @export
createGrowthObject <- function(document, type = 'fast', samples = 1000) {

  futile.logger::flog.info(paste('...preparing growth object for', document$fileDesc),
            name = 'green')

  # Read data and create document frequency matrix
  textData <- unlist(readFile(document))
  tokens <- unlist(quanteda::tokenize(textData, what = "word"))
  sampleSize <- floor(length(tokens) / samples)

  if (type == 'fast') {

    # Initialize VGC vectors an data
    N <- numeric(length = samples)
    V <- numeric(length = samples)
    vocabulary <- ''

    # Build vocabulary growth curve data
    for (i in 1:samples) {
      from <- 1 + ((i-1)*sampleSize)
      to <- i * sampleSize
      vocabulary <- unique(c(vocabulary, unique(tokens[from:to])))
      N[i] <- i * sampleSize
      V[i] <- length(vocabulary)
    }

    vgc <- data.frame(N = N, V = V)
  } else {
    vgc <- languageR::growth.fnc(text = textData, size = sampleSize, nchunks = samples)
  }
  return(vgc)
}

#==============================================================================#
#                                  createVGC                                   #
#==============================================================================#
#'  createVGC
#'
#'  \code{createGrowthObject} Creates vocabulary growth data for a corpus
#'
#' This function takes as its parameter the korpus meta data and type of
#' analysis to conduct and returns vocabulary growth data indicating how the
#' vocabulary grows in terms of new unique tokens as the V
#'
#' @param korpus - the meta data for the korpus being analyzed
#' @param type = c('fast', 'full)
#' @return growth - list of growth objects
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family vocabulary growth functions
#' @export
createVGC <- function(korpus, type = 'fast') {

  startTime <- Sys.time()
  futile.logger::flog.info(paste('Preparing vocabulary growth curve data for',
                  korpus$corpusName), name = 'green')

  vgc <- lapply(seq_along(korpus$documents), function(x) {
    createGrowthObject(korpus$documents[[x]], type = type)
  })

  # Log
  endTime <- Sys.time()
  futile.logger::flog.info(paste('Vocabulary growth objects created. Elapsed time is',
                  format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
            name = 'green')

  return(vgc)
}
## ---- end

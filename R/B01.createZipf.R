## ---- create_zipf_objects

#==============================================================================#
#                            createZipfObject                                  #
#==============================================================================#
#'  createZipfObject
#'
#'  \code{createZipfObject} Prepares zipf objects for a document
#'
#' This function takes as its parameters, the list containing the document
#' and its meta data, a vocabulary growth curve, the number of samples
#' (default = 1000),  and returns a zip object used for downstream analysis.
#' The zipf object includes:
#' \itemize{
#'  \item{nTokens}{Number of tokens in the document}
#'  \item{docTfl}{Document term frequency list}
#'  \item{docSpc}{Document frequency spectrum object}
#'  \item{docLnre}{Large number of rare elements (LNRE) object}
#'  \item{docEmpVgc}{Empirical (observed) vocabulary growth curve}
#'  \item{docIntVgc}{Interpolated (smoothed) vocabulary growth curve}
#'  \item{goodness}{LNRE goodness of fit}#'
#' }
#'
#' @param document - the document and meta data to be analyzed
#' @param vgc - the vocabulary growth curve data
#' @param numSamples - the number of samples, default = 1000
#' @return zipfObjects - the zipf object used for analysis and plotting
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @seealso \code{\link{createVGC}}
#'          \code{\link{createGrowthObject}}
#'          \code{\link{createZipf}}
#'          \code{\link[languageR]{text2spc.fnc}}
#'          \code{\link[zipfR]{lnre}}
#'          \code{\link[zipfR]{lnre.spc}}
#'          \code{\link[zipfR]{N}}
#'          \code{\link[zipfR]{V}}
#'          \code{\link[zipfR]{EV}}
#' @export
createZipfObject <- function(document, vgc, numSamples = 1000) {

  futile.logger::flog.info(paste('...preparing zipf object for',
                                 document$fileDesc), name = 'green')

  # Read document and extract words
  nTokens <- length(document$data)
  sampleSize <- floor(nTokens / numSamples)

  # Create frequency spectrum object and term frequency list
  docSpc    <- languageR::text2spc.fnc(document$data)
  docSpc$m  <- as.integer(docSpc$m)
  docSpc$Vm <- as.integer(docSpc$Vm)
  docTfl    <- zipfR::spc2tfl(docSpc)

  # Prepare Empirical Vocabulary Growth Curve
  docEmpVgc <- vgc

  # Calculate N
  if ('data.frame' %in% class(vgc)) {
    N = subset(docEmpVgc, select = N)$N
  } else {
    N = tail(docEmpVgc@data$data$Tokens, 1)
  }

  # Prepare an interpolated vocabulary growth curve
  docIntVgc <- zipfR::vgc.interp(docSpc, N, m.max=1, allow.extrapolation=TRUE)

  # Prepare extrapolated vocabulary growth curve
  docLnre <- zipfR::lnre('zm', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  docExtVgc <- zipfR::lnre.vgc(docLnre, (1:numSamples)*sampleSize)

  # Calculate goodness of fit
  g <- zipfR::lnre.goodness.of.fit(docLnre, docSpc, n.estimated=0, m.max=15)
  goodness <- data.frame(Register = document$fileDesc, X2 = g$X2, P = g$p)

  zipfObject <- list(
    category = document$fileDesc,
    nTokens = nTokens,
    docTfl = docTfl,
    docSpc = docSpc,
    docLnre = docLnre,
    docEmpVgc = docEmpVgc,
    docIntVgc = docIntVgc,
    docExtVgc = docExtVgc,
    goodness = goodness)

  return(zipfObject)
}

#==============================================================================#
#                               createZipf                                     #
#==============================================================================#
#'  createZipf
#'
#'  \code{createZipf} Prepares zipf objects for a corpus
#'
#' This function takes as its parameters, the corpus meta data and a
#' vocabulary growth curve and returns a zip object used for downstream
#' analysis. The zipf object includes:
#' \itemize{
#'  \item{nTokens}{Number of tokens in the document}
#'  \item{docTfl}{Document term frequency list}
#'  \item{docSpc}{Document frequency spectrum object}
#'  \item{docLnre}{Large number of rare elements (LNRE) object}
#'  \item{docEmpVgc}{Empirical (observed) vocabulary growth curve}
#'  \item{docIntVgc}{Interpolated (smoothed) vocabulary growth curve}
#'  \item{goodness}{LNRE goodness of fit}#'
#' }
#'
#' @param korpus - meta data for the corpus being analyzed
#' @param vgc - the vocabulary growth curve data
#' @return zipfObjects - the zipf object used for analysis and plotting
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @seealso \code{\link{createVGC}}
#'          \code{\link{createGrowthObject}}
#'          \code{\link{createZipfObject}}
#' @export
createZipf <- function(korpus, vgc) {

  # Prepare opening log entry
  startTime <- Sys.time()
  futile.logger::flog.info(paste('Creating Zipf objects for', korpus$corpusName),
                           name = 'green')


  zipfObjects <- lapply(seq_along(korpus$documents), function (x) {
    document <- readFile(korpus$documents[[x]])
    korpus$documents[[x]]$data <- unlist(quanteda::tokenize(document,
                                                            what = "word"))
    createZipfObject(korpus$documents[[x]], vgc[[x]])
  })

  # Prepare closing log entry
  endTime <- Sys.time()
  msg <- paste('Zipf objects created for', korpus$corpusName,
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')


  return(zipfObjects)
}
## ---- end

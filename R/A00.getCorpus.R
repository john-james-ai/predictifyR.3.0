## ---- get_data
#==============================================================================#
#                                 getCorpus                                    #
#==============================================================================#
#'  getCorpus
#'
#'  \code{getCorpus} obtains the HC Corpora from the website
#'
#'  The function obtains the HC Corpora data from its online source if the
#'  document doesn't already exist
#'
#'
#' @param force = Logical. Force download and overwrite local copy if TRUE
#' @return sysData Internal data and data structures
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @family text processing functions
#' @export
getCorpus <- function(force = FALSE) {

  # Set time and log entry
  startTime <- Sys.time()
  # Set logger parameters
  futile.logger::flog.threshold(INFO)
  futile.logger::flog.logger("green", INFO, appender=appender.file('./log/green.log'))
  futile.logger::flog.logger("yellow", INFO, appender=appender.tee('./log/yellow.log'))
  futile.logger::flog.logger("red", INFO, appender=appender.tee('./log/red.log'))
  futile.logger::flog.info(paste("Entering function: getCorpus",
                                 "Obtaining raw data"), name = 'green')

  # Download data
  if (dir.exists(corpora$raw$directory) &
      checkDir(corpora$raw$directory) == 3 &
      force == FALSE) {
    futile.logger::flog.info("HC Corpus already exists, not downloaded / overwritten")
  } else {
    downloadFile <- tempfile()
    download.file(corpora$raw$url, destfile = downloadFile, mode = 'wb')
    unzip(zipfile = downloadFile, overwrite = FALSE,
          exdir = corpora$raw$directory,
          junkpaths = TRUE, files = corpora$raw$registers)
    unlink(downloadFile)
  }

  # Create log entry
  endTime <- Sys.time()
  msg <- paste('Exiting function: getCorpus. HC Corpora obtained.',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  futile.logger::flog.info(msg, name = 'green')
}


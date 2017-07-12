## ---- get_data
#==============================================================================#
#                                 getData                                      #
#==============================================================================#
#'  getData
#'
#'  \code{getData} obtains the HC Corpora from the website
#'
#'  The function obtains the HC Corpora data from its online source if the
#'  document doesn't already exist
#'
#'
#' @param force = Logical. Force download and overwrite local copy if TRUE
#' @return sysData Internal data and data structures
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
getData <- function(force = FALSE) {

  # Set time and log entry
  startTime <- Sys.time()
  flog.threshold(INFO)
  flog.logger("green", INFO, appender=appender.file('./log/green.log'))
  flog.logger("yellow", INFO, appender=appender.tee('./log/yellow.log'))
  flog.logger("red", INFO, appender=appender.tee('./log/red.log'))
  flog.info("Obtaining raw data", name = 'green')

  # Download data
  if (dir.exists(corpora$raw$directory) &
      checkDir(corpora$raw$directory) == 3 &
      force == FALSE) {
    flog.info("HC Corpus already exists, not downloaded / overwritten")
  } else {
    downloadFile <- tempfile()
    download.file(corpora$raw$url, destfile = downloadFile, mode = 'wb')
    unzip(zipfile = downloadFile, overwrite = FALSE,
          exdir = corpora$raw$directory,
          junkpaths = TRUE, files = corpora$raw$registers)
  }

  # Create log entry
  endTime <- Sys.time()
  msg <- paste('HC Corpora obtained.',
               'Elapsed time is',
               format(round(difftime(endTime, startTime,  units = 'auto'),
                            2)))
  flog.info(msg, name = 'green')
}


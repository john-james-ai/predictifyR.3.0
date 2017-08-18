## ---- build_pilot
#------------------------------------------------------------------------------#
#                              buildPilot                                      #
#------------------------------------------------------------------------------#
#'  buildPilot
#'
#'  \code{buildPilot} Builds pilot corpus for analysis
#'
#' This function takes as its parameters, the meta data for the clean and
#' pilot corpora, and the pilot corpus design and  creates a pilot corpus
#' according to the design and stores it for downsream analysis.
#'
#' @param clean List with meta data for clean corpus
#' @param pilot  List with meta data for pilot corpus
#' @param design Dataframe containing the pilot corpus design
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
buildPilot <- function(clean, pilot, design) {

  startTime <- Sys.time()
  futile.logger::flog.info("Building pilot corpus", name = 'green')

  korpus <- lapply(seq_along(clean$documents), function(r) {
    readFile(clean$documents[[r]])
  })

  lapply(seq_along(korpus), function(d) {
    futile.logger::flog.info(paste("...building", pilot$documents[[d]]$fileDesc),
                             name = 'green')

    chunks <- design$pilot$`# Chunks`[d]
    chunkSize <- floor(design$pilot$`Sample Size (Sentences)`[d] / design$pilot$`# Chunks`[d])
    # Sample the data
    chunks <- sampleData(korpus[[d]], chunks, chunkSize, format = 'v')

    # Save the file
    pilot$documents[[d]]$data <- chunks
    saveFile(pilot$documents[[d]])
  })

  # Closing log
  endTime <- Sys.time()
  futile.logger::flog.info(
    paste('Pilot corpus build complete. Elapsed time is',
          format(round(difftime(endTime, startTime,  units = 'auto'), 2))),
    name = 'green')

}
## ---- end

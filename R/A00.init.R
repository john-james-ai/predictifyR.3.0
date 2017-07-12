## ---- init
#==============================================================================#
#                                 init                                         #
#==============================================================================#
#'  init
#'
#'  \code{init} initialization function
#'

#'
#' @return sysData Internal data and data structures
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
init <- function(force = FALSE) {

  # Create logger
  flog.threshold(INFO)
  flog.logger("green", INFO, appender=appender.file('./log/green.log'))
  flog.logger("yellow", INFO, appender=appender.tee('./log/yellow.log'))
  flog.logger("red", INFO, appender=appender.tee('./log/red.log'))
  flog.info("Let's get our NLP on shall we?", name='green')



}

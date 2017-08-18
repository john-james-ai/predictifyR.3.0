## ---- Platform
#==============================================================================#
#                                 Platform                                     #
#==============================================================================#
#' Platform
#'
#'
#' \code{Platform} Class the creates and manages environments.
#'
#' This class creates and manages environments.  An environment is essentially
#' a directory in which project data resides.  Multiple environments can
#' be created to provide multiple data environments. This is especially
#' useful for projects with large data sets or computationally expense
#' analyses.
#'
#' @docType class
#' @examples
#' \dontrun{
#' predictor$new(pName = "predictor", pDesc = "Platform for predictor project")
#' predictor$createEnv(envName = "test", envDesc = "Test Environment",
#'                     envPath = "test", envStatus = TRUE)
#' predictor$archiveEnv(name = 'test')
#' predictor$listEnv()
#' predictor$currentEnv
#' predictor$currentEnv <- "production"
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(pName, pDesc)}}{Initializes the platform}
#'  \item{\code{createEnv(envName, envDesc, envPath, envStatus)}}{Creates an environment}
#'  \item{\code{archiveEnv(environment)}}{Archives an environment}
#'  \item{\code{listEnv()}}{Provides a data frame of environments in the platform}
#' }
#'
#' @section Active Bindings:
#' \describe{
#'  \item{\code{currentEnv}}{Gets and sets the current environment}
#' }
#'
#'
#' @return Object of \code{\link{R6Class}} with methods for managing environments
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
Platform = R6::R6Class("Platform",
 private = list(
   ..pName = character(0),
   ..pDesc = character(0),
   ..pPath = character(0),
   ..environments = data.frame(),
   ..currentEnv = data.frame(),
   ..pCreated = character(0),

   setupLogs = function(envPath) {

     logDir <- file.path(paths$environments, envPath, 'logs')

     futile.logger::flog.threshold(INFO)
     futile.logger::flog.logger(
       "green",INFO, appender=appender.file(paste0(logDir, "/", "green.log")))
     futile.logger::flog.logger(
       "yellow",WARN, appender=appender.tee(paste0(logDir, "/", "yellow.log")))
     futile.logger::flog.logger(
       "red",ERROR, appender=appender.tee(paste0(logDir, "/", "red.log")))
   },

   validateEnv = function(envName) {

     # Suppress automatically generated error messages
     opt <- options(show.error.messages=FALSE)
     on.exit(options(opt))

     assertive.strings::assert_all_strings_are_not_missing_nor_empty(envName)
     if (envNames == "") {
       futile.logger::flog.error(paste(
         "Error in Platform class, validateEnv method",
         "Environment name must not be blank"),
         name = 'red')
       stop()
     }
   },

   existEnvObject = function(eName) {
     if (nrow(subset(private$..environments,
                     private$..environments$envName == eName)) == 0) {
       return(FALSE)

     } else {
       return(TRUE)
     }
   }
 ),

 active = list(
   pName = function() { private$..pName },
   pDesc = function() { private$..pDesc},
   pPath = function() { private$..pPath},
   pCreated = function() { private$..pCreated},


   currentEnv = function(value) {

     if (missing(value)) {
       private$..currentEnv

     } else if (private$existEnvObject(value) == FALSE) {

       futile.logger::flog.error(paste(
         "Error in Platform class, currentEnv method",
         "Environment does not exist"),
         name = 'red')

     } else {
       private$..environments$envStatus <- FALSE
       private$..environments$envStatus[private$..environments$envName == value] <- TRUE
       private$..currentEnv <- subset(private$..environments,
                                      private$..environments$envStatus == TRUE)
     }
   }
 ),

 public = list(

   initialize = function(pName, pDesc = NULL) {

     # Suppress automatically generated error messages
     opt <- options(show.error.messages=FALSE)
     on.exit(options(opt))

     # Confirm platform object does not exist
     p <- mget(pName, inherits = FALSE, ifnotfound = c(""))
     if (class(p[[1]]) != "character") {
       futile.logger::flog.error(paste(
         "Error in Platform class, initialize method.",
         "\nPlatform", pName, "already exists."),
         name = "red")
       stop()
     }

     # Confirm platform directory does not exist
     pPath <-  gsub(pattern = " ", replacement = "-", pName)
     if (dir.exists(pPath) == TRUE) {
       futile.logger::flog.error(paste(
         "Error in Platform class, initialize method.",
         "\nPlatform directory", pPath, "already exists."),
         name = "red")
       stop()
     }

     # Validate name
     assertive.strings::assert_is_a_non_missing_nor_empty_string(pName)

     # Create Directory Structure
     home <- file.path('./PLATFORMS', pName)
     envHome <- file.path(home, "environments")
     envDir <- file.path(envHome, pName)
     dataDir <- file.path(envDir, 'data')
     modelDir <- file.path(envDir, 'model')
     logsDir <- file.path(envDir, 'logs')
     archives <- file.path(home, "archives")

     dir.create(home, recursive = TRUE)
     dir.create(envHome, recursive = TRUE)
     dir.create(envDir, recursive = TRUE)
     dir.create(dataDir, recursive = TRUE)
     dir.create(modelDir, recursive = TRUE)
     dir.create(logsDir, recursive = TRUE)
     dir.create(archives, recursive = TRUE)

     # Create platform object
     private$..pName <- pName
     private$..pDesc <- pDesc
     private$..pPath <- paste0('./', pName)
     private$..environments <- .pkgGlobalEnv$env
     private$..currentEnv <- subset(.pkgGlobalEnv$env, .pkgGlobalEnv$envStatus == TRUE)
     private$..created <- Sys.time()

     invisible(self)

   },

   createEnv = function(eName, eDesc = "", eStatus = FALSE) {

     # Suppress automatically generated error messages
     opt <- options(show.error.messages=FALSE)
     on.exit(options(opt))

     # First level validation
     assertive.strings::assert_all_strings_are_not_missing_nor_empty(eName)
     assertive.strings::assert_all_strings_are_not_missing_nor_empty(eDesc)
     assertive.types::is_a_bool(eStatus)

     # Second level validation
     # Confirm environment doesn't exist
     if (private$existEnvObject(eName) == TRUE) {
       futile.logger::flog.error(paste(
         "Error in Platform class, createEnv method",
         "Environment", eName, "already exists."),
         name = "red")
       stop()
     }

     # Confirm environment directory does not exist
     ePath <- gsub(pattern = " ", replacement = "-", eName)
     ePath <- file.path(private$pName, paths$environments, ePath)

     if (dir.exists(ePath) == TRUE) {
       futile.logger::flog.error(paste(
         "Error in Platform class, initialize method",
         "Directory", ePath, "already exists.",
         "Delete/move the directory or rebuild this one"),
         name = "red")
       stop()
     }

     # Format
     if (is.na(eDesc) | is.null(eDesc) |
         !is.character(eDesc) | eDesc == "") {
       eDesc <- paste(eName, "environment")
     }


     # Update environment data frame
     e <- data.frame(envName = envName, envDesc = envDesc, envPath = envPath,
                     envStatus = envStatus, envCreated = Sys.time())

     if (eStatus == TRUE) { private$..environments$envStatus <- FALSE }
     private$..environments <- rbind(private$..environments, e)
     private$..currentEnv <- subset(private$..environments,
                                    private$..environments$envName == eName)
     invisible(self)

   },

   archiveEnv = function(eName) {

     e <- subset(private$..environments,
                 private$..environments$envName == eName)

     envName <- paste0(sub('\\..*', '', paste0(e$envName)),
                            format(Sys.time(),'_%Y%m%d_%H%M%S'))

     envDesc <- paste(sub('\\..*', '', paste0('Archived ',e$envDesc)),
                           format(Sys.time(),'%Y%m%d_%H%M%S'))
     envPath <- e$envPath
     envStatus <- FALSE

     self$createEnv(envName, envDesc, envStatus)

     # Copy data to archive
     base::dir.create(file.path(env$archives, e$envPath), recursive = TRUE)
     base::file.copy(file.path(env$environments, e$envPath),
                     file.path(env$archives, e$envPath), recursive = TRUE)

   },

   listEnv = function() { private$..environments }
 ),
 lock_objects = FALSE
)

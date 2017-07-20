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
#' be created to provide multiple versions of the data. This is especially
#' useful for projects with large data sets. Users can create environments
#' to contain smaller versions of the data sets for testing purposes.
#'
#' @docType class
#' @examples
#' \dontrun{
#' Platform$new(name = "predictor", desc = "Predictor platform")
#' predictor$printPlatform()
#' predictor$createEnv(name = "test", desc = "Test Environment", path = "test", current = TRUE)
#' predictor$archiveEnv(name = 'test')
#' predictor$existsEnv(name = 'production')
#' predictor$listEnv()
#' predictor$currentEnv
#' predictor$currentEnv <- "production"
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(name, desc)}}{Initializes the platform}
#'  \item{\code{printPlatform()}}{Prints the platform object definition}
#'  \item{\code{createEnv(name, desc, path, current)}}{Creates an environment}
#'  \item{\code{archiveEnv(name)}}{Archives an environment}
#'  \item{\code{existsEnv(name)}}{Checks existence of an environment}
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
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Platform = R6::R6Class("Platform",
 private = list(
   ..name = character(0),
   ..desc = character(0),
   ..currentEnv = data.frame(),
   ..environments = data.frame(),
   ..created = character(0)
 ),

 active = list(
   currentEnv = function(value) {
     if (missing(value)) {
       private$..currentEnv
       } else {
         # Validate
         assertive.types::is_character(value)
         if (nrow(subset(private$..environments, Name == value)) == 0) {
           futile.logger::flog.error(paste(
             "Error in currentEnv. Invalid environment."),
             name = 'red')
         } else {
           private$..environments$Current <- FALSE
           private$..environments$Current[private$..environments$Name == value] <- TRUE
         }
       }
   }
 ),

 public = list(
   initialize = function(name, desc) {

     # Check if exists
     check <- mget(name, inherits = TRUE, ifnotfound = list('Platform not found'))
     if (!grepl('not found', check[[1]], perl = TRUE)) {
       futile.logger::flog.warn(paste0(
         "Platform ", name, " already exists.  Choose another name or access ",
         name," by using ", name, ".method notation."),
         name = 'yellow')
     } else {

       # Validate
       assertive.types::is_character(name)
       assertive.types::is_character(desc)

       # Create initial environment
       envName <- 'main'
       envDesc <- ' main environment'
       envPath <- file.path('main')
       Environment$new(envName, envDesc, envPath)
       env <- data.frame(Name = envName, Desc = envDesc, Path = envPath,
                         Current = TRUE, Created = Sys.time(),
                         row.names = NULL)

       # Instantiate platform
       private$..name <- name
       private$..desc <- desc
       private$..currentEnv <- env
       private$..environments <- env
       private$..created <- Sys.time()
     }
   },

   printPlatform = function() {
     cat(paste('\nPlatform Name: '), private$..name)
     cat(paste('\nPlatform Description: '), private$..desc)
     cat(paste('\nCurrent Environment:\n\n'))
     print.data.frame(private$..currentEnv)
     cat(paste('\nPlatform Created: '), as.character(
        as.POSIXct(private$..created, origin="1970-01-01"), usetz=T), '\n\n')
   },

   createEnv = function(name, desc, path, current){

     # Validate
     assertive.types::is_character(name)
     assertive.types::is_character(desc)
     assertive.types::is_character(path)
     assertive.types::is_a_bool(current)

     # Create  environment
     Environment$new(name, desc, path)

     # Update current environment if current = TRUE
     if (current == TRUE) {
       private$..environments$Current = FALSE
     }
     env <- data.frame(Name = name, Desc = desc, Path = path, Current = current,
                       Created = Sys.time(), row.names = NULL)
     private$..environments <- rbind(private$..environments, env)
     private$..currentEnv <- env
     invisible(self)
   },

   archiveEnv = function(envName) {

     e <- subset(private$..environments, private$..environments$Name == envName)
     name <- paste0(sub('\\..*', '', paste0(e$Name)),
                            format(Sys.time(),'_%Y%m%d_%H%M%S'))

     desc <- paste(sub('\\..*', '', paste0('Archived ',e$Desc)),
                           format(Sys.time(),'%Y%m%d_%H%M%S'))
     path <- e$Path
     current = FALSE

     self$createEnv(name, desc, path, current)

     # Copy data to archive
     base::dir.create(file.path('./archive', e$Path), recursive = TRUE)
     base::file.copy(file.path('environments',e$Path),
                     file.path('./archive', e$Path), recursive = TRUE)

   },

   existsEnv = function(envName = NULL) {

     if (length(envName) == 0) {
       futile.logger::flog.warn(paste(
         'Error in existsEnv.', 'No environment entered'),
         name = 'yellow')
     } else {

       if (nrow(private$..environments) == 0) {
         return(FALSE)
       } else if (nrow(subset(private$..environments,
                              private$..environments$Name == envName)) == 0) {
         return(FALSE)
       } else {
         return(TRUE)
       }
     }
   },

   listEnv = function() {
       private$..environments
     }
 ),
 lock_objects = FALSE
)

# rm(predictifyR)
# predictifyR <- Platform$new(name = 'predictifyR', 'PredictifyR Platform')
# predictifyR$listEnv()
# predictifyR$existsEnv('main')
# predictifyR$existsEnv('dasd')
# predictifyR$printPlatform()
# predictifyR$createEnv('test', 'Test Environment', 'test', TRUE)
# predictifyR$listEnv()
# predictifyR$printPlatform()
# predictifyR$currentEnv

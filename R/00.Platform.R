Platform = R6::R6Class("Platform",
 private = list(
   ..name = character(0),
   ..desc = character(0),
   ..currentEnv = NULL,
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
         if (nrow(subset(private$..environments, name == value)) == 0) {
           futile.logger::flog.error(paste(
             "Error in currentEnv. Invalid environment."),
             name = 'red')
         } else {
           private$..environments$current <- FALSE
           private$..environments$current[private$..environments$name == value] <- TRUE
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
       envDesc <- paste(name,' main environment')
       envPath <- file.path('main')
       env <- Environment$new(envName, envDesc, envPath)

       # Instantiate platform
       private$..name <- name
       private$..desc <- desc
       private$..currentEnv <- envName
       private$..environments <- data.frame(Name = envName,
                                           Desc = envDesc,
                                           Path = envPath,
                                           Current = TRUE,
                                           Created = Sys.time())
       private$..created <- Sys.time()
     }
   },

   printPlatform = function() {

     cat(paste('\nPlatform Name: '), private$..name)
     cat(paste('\nPlatform Description: '), private$..desc)
     cat(paste('\nCurrent Environment: '), private$..currentEnv)
     cat(paste('\nCreated: '), as.character(
       as.POSIXct(private$..created, origin="1970-01-01"), usetz=T), '\n\n')
   },

   createEnv = function(name, desc, path, current){

     # Validate
     assertive.types::is_character(name)
     assertive.types::is_character(desc)
     assertive.types::is_character(path)
     assertive.types::is_a_bool(current)

     # Create initial environment
     Environment$new(name, desc, path)

     # Update current environment if current = TRUE
     if (current == TRUE) {
       private$..environments$Current = FALSE
     }
     env <- list()
     env$Name <-  name
     env$Desc <- desc
     env$Path <- path
     env$Current <- current
     env$Created <- Sys.time()
     private$..environments <- rbind(private$..environments, rbindlist(list(env)))
     invisible(self)
   },

   archiveEnv = function(envName) {

     e <- subset(private$..environments, private$..environments$name == envName)
     name <- paste0(sub('\\..*', '', paste0(e$Name)),
                            format(Sys.time(),'_%Y%m%d_%H%M%S'))

     desc <- paste(sub('\\..*', '', paste0('Archived ',e$Desc)),
                           format(Sys.time(),'%Y%m%d_%H%M%S'))
     path <- e$Path
     current = FALSE

     self$createEnv(name, desc, path, current)

     # Copy data to archive
     base::dir.create(file.path('./archive', e$path), recursive = TRUE)
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

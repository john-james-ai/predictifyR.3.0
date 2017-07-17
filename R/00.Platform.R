Platform <- R6Class ("Platform",
 private = list(
   ..environments = data.frame()
 ),
 public = list(
   initialize = function(environment) {

     # Validate parameters
     assertive.types::is_list(environment)
     assertive.properties::is_non_empty(environment$name)
     assertive.properties::is_non_empty(environment$desc)
     assertive.types::assert_is_character(environment$name)
     assertive.types::assert_is_character(environment$desc)
     assertive.files::is_dir(environment$path)

     # Initialize Platform with environment and set current
     environment$current = TRUE
     environment$created = Sys.time()
     private$..environments = rbindlist(list(environment))
     invisible(self)
   },

   existsEnv = function(env) {
     if (nrow(subset(private$..environments, private$..environments$name == env)) == 0) {
       return(FALSE)
     } else {
       return(TRUE)
     }
   },

   addEnv = function(environment, makeCurrent) {

     if (!self$existsEnv(environment$name)) {

       # Validate parameters
       assertive.types::is_list(environment)
       assertive.properties::is_non_empty(environment$name)
       assertive.properties::is_non_empty(environment$desc)
       assertive.types::assert_is_character(environment$name)
       assertive.types::assert_is_character(environment$desc)
       assertive.files::is_dir(environment$path)

       # Add environment to list of environments and set current
       if (makeCurrent == TRUE) {
         private$..environments$current = FALSE
         environment$current = TRUE
       } else {
         environment$current = FALSE
       }

       environment$created = Sys.time()
       env = rbindlist(list(environment))
       private$..environments = rbind(private$..environments, env)
       invisible(self)

     } else {
       futile.logger::flog.warn(paste(environment$name,"already exists"),
                                name = 'yellow')
     }
   },

   getEnvs = function() {
     private$..environments
   },

   archiveEnv = function(env) {

     if (self$existsEnv(env) == TRUE) {

       # Create archive environment from named environment
       e <- subset(private$..environments, private$..environments$name == env)
       archive <- e
       archive$name = paste0(sub('\\..*', '', paste0(env)),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'))

       archive$desc = paste(sub('\\..*', '', paste0('Archived ',e$desc)),
                             format(Sys.time(),'%Y%m%d_%H%M%S'))
       archive$path = file.path('./archive', archive$name)
       self$addEnv(archive, makeCurrent = FALSE)

       # Copy data to archive
       base::dir.create(archive$path, recursive = TRUE)
       base::file.copy(e$path, archive$path, recursive=TRUE)

     } else {
       futile.logger::flog.warn(paste(name,"is not a valid environment"),
                                name = 'yellow')
     }
   }

 ),
 active = list(
   currentEnv = function(value) {
     if (missing(value)) {
       return(subset(private$..environments,
                     private$..environments$current == TRUE))
     } else {
       if (self$existsEnv(value) == TRUE) {
         private$..environments$current = FALSE
         private$..environments = within(private$..environments,
                                         current[name == value] <- TRUE)
       } else {
         futile.logger::flog.warn(paste(value,"is not a valid environment"),
                                        name = 'yellow')
       }
     }
   }
 ),
 lock_objects = FALSE
)

Korpus = R6::R6Class("Korpus",
  private = list(
    ..environments = data.frame(),
    ..korpus = list(
        name = character(0),
        desc = character(0),
        path = character(0),
        url = character(0),
        files = character(0),
        created = NULL,
        documents = list(
          name = character(0),
          desc = character(0),
          path = character(0),
          documents = list()
      )
    ),

  #---------------------------------------------------------------------------#
  #                         Private Corpus Methods                            #
  #---------------------------------------------------------------------------#

    parseWhat = function(what) {

      if (length(what) == 0) {
        futile.logger::flog.error(paste(
          "Error in parseWhat.",
          "Value for 'what' is missing with no default."),
          name = 'red')
        stop()
      }

      if (length(private$..korpus[[what]]) == 0) {
        futile.logger::flog.error(paste(
          "Error in parseWhat.", what, "is an invalid content type"),
                                 name = 'red')
        stop()
      }

      if (length(private$..korpus[[what]]$path) != 0 &
          length(private$..korpus[[what]]$fileName) != 0) {
        res <- list()
        res$path <- private$..korpus[[what]]$path
        res$fileName <- private$..korpus[[what]]$fileName

      } else if (length(private$..korpus[[what]]$documents) != 0) {
        res <- lapply(seq_along(private$..korpus[[what]]$documents), function(x) {
          r <- list()
          r$path <-
        }
        res <- private$..korpus[[what]]$documents
      } else if (dir.exists(what)) {

      }


      }


    }


    checkDir = function(directory) {

      if (!dir.exists(directory)) {
        dir.create(directory, recursive = TRUE)
        dirSize = 0
      } else {
        dirSize = length(dir(path = directory, all.files = FALSE))
      }
      return(dirSize)
    },

    shared = {
      e <- new.env()
      e$currentEnv <- NULL
      e
    }

  ),

  #---------------------------------------------------------------------------#
  #                         Active Bindings                                   #
  #---------------------------------------------------------------------------#

  active = list(
    currentEnv = function(value) {
      if (missing(value)) {
        private$shared$currentEnv
      } else {

        # Validate environment variable
        assertive.types::is_data.frame(value)
        assertive.properties::is_non_empty(value$name)
        assertive.properties::is_non_empty(value$desc)
        assertive.properties::is_non_empty(value$path)

        assertive.types::is_data.frame(value)
        assertive.types::assert_is_character(value$name)
        assertive.types::assert_is_character(value$desc)
        assertive.files::is_dir(value$path)

        if (self$existsEnv(value$name)) {
          private$shared$currentEnv <- value
        } else {
          futile.logger::flog.error("Invalid environment passed to currentEnv.",
                                    name = 'red')
          stop()
        }
      }
    }
  ),

  #---------------------------------------------------------------------------#
  #                         Public Corpus Methods                             #
  #---------------------------------------------------------------------------#

  public = list(
    initialize = function(korpus) {

      # Validate corpus meta data
      assertive.properties::is_non_empty(korpus$meta$name)
      assertive.properties::is_non_empty(korpus$meta$desc)
      assertive.properties::is_non_empty(korpus$meta$path)

      if (length(korpus$meta$url) != 0) {
        if (!RCurl::url.exists(korpus$meta$url)) {
          futile.logger::flog.error(paste(korpus$meta$url,
                                          "is not a valid url."),
                                    name = 'red')
          stop()
        }
      }
      assertive.types::assert_is_character(korpus$meta$name)
      assertive.types::assert_is_character(korpus$meta$desc)
      assertive.files::is_dir(korpus$meta$path)

      # Validate register meta data
      assertive.properties::is_non_empty(korpus$registers$name)
      assertive.properties::is_non_empty(korpus$registers$desc)
      assertive.properties::is_non_empty(korpus$meta$path)
      assertive.types::assert_is_character(korpus$registers$name)
      assertive.types::assert_is_character(korpus$registers$desc)
      assertive.files::is_dir(korpus$registers$path)

      # Validate register document meta data
      if (length(korpus$registers$documents) == 0) {
        futile.logger::flog.error("Documents list is empty",
                                  name = 'red')
        stop()
      }
      lapply(seq_along(korpus$registers$documents), function(x) {
        assertive.properties::is_non_empty(korpus$registers$documents[[x]]$name)
        assertive.properties::is_non_empty(korpus$registers$documents[[x]]$desc)
        assertive.properties::is_non_empty(korpus$registers$documents[[x]]$obj)
        assertive.types::assert_is_character(korpus$registers$documents[[x]]$name)
        assertive.types::assert_is_character(korpus$registers$documents[[x]]$desc)
        assertive.types::assert_is_character(korpus$registers$documents[[x]]$obj)
      })

      # Instantiate Meta Data
      private$..korpus$meta$name   = korpus$meta$name
      private$..korpus$meta$desc   = korpus$meta$desc
      private$..korpus$meta$path   = korpus$meta$path
      private$..korpus$meta$url    = korpus$meta$url
      private$..korpus$meta$files  = korpus$meta$files
      private$..korpus$meta$created = Sys.time()

      # Instantiate registers Data
      private$..korpus$registers$name   = korpus$registers$name
      private$..korpus$registers$desc   = korpus$registers$desc
      private$..korpus$registers$path   = korpus$registers$path
      private$..korpus$registers$documents  = korpus$registers$documents


    },

    readBinary = function(what) {
      readBin(file.path(fileHandle$path, fileHandle$name), raw(),
              file.info(file.path(fileHandle$path, fileHandle$name))$size)
    },

    readText = function(what) {
      con <- file(file.path(fileHandle$path, fileHandle$name))
      on.exit(close(con))
      readLines(con)
    },

    readRData = function(what) {
      env <- new.env()
      object <- load(file.path(fileHandle$path, fileHandle$name), envir = env)
      return(env[[object]])
    },

    read = function(what) {

      order <- private$parseWhat(what)

      # Confirm content type is available
      if (length(private$..korpus[[what]]) == 0) {
        futile.logger::flog.error(paste(what,"is not a valid content type"),
                                 name = 'red')
      }

      # Format enquiry

      # Read text
      document = lapply(seq_along(content$documents), function(x) {
        f <-  list()
        f$path = file.path(env$path, private$..korpus$path, content$path)
        message('path is:', f$path)
        f$name = content$documents[[x]]$name
        f$obj  = content$documents[[x]]$obj
        self$readText(f)
      })
    },

    strCorpus = function() {

      # Print corpus meta data
      cat(paste('\nCorpus Name: '), private$..korpus$meta$name)
      cat(paste('\nCorpus Description: '), private$..korpus$meta$desc)
      cat(paste('\nCorpus Path: '), private$..korpus$meta$path)

      if (length(private$..korpus$meta$url) > 0) {
        cat(paste('\nCorpus URL: '), private$..korpus$meta$url)
      }

      if (length(private$..korpus$meta$files) > 0) {
        cat(paste('\nCorpus Files: '), private$..korpus$meta$files)
      }

      cat(paste('\nCorpus Created: '),
          as.character(as.POSIXct(private$..korpus$meta$created,
                                  origin="1970-01-01"),usetz=T), '\n\n')

      # Print registers meta data
      cat(paste('\nRegisters Name: '), private$..korpus$registers$name)
      cat(paste('\nRegisters Description: '), private$..korpus$registers$desc)
      cat(paste('\nRegisters Path: '), private$..korpus$registers$path)
      cat(paste('\nRegisters Type: '), private$..korpus$registers$type, '\n\n')

      # Print registers document definitions
      documents <- rbindlist(lapply(seq_along(private$..korpus$registers$documents),
                                    function(x) {
           private$..korpus$registers$documents[[x]]
      }))
      names(documents) <- c('Description', 'File Name', 'Object')
      print(documents)
    },

    download = function(env) {

      # Create file path for the environment
      fp = private$getPath(env)

      # If directory is not empty do not download!
      if (length(dir(path = fp, all.files = FALSE)) > 0) {
        futile.logger::flog.warn(paste("Download directory not empty.",
                                       "Corpus not downloaded/overwritten.",
                                       "Delete the directory or select another",
                                       "environment"),
                                 name = 'yellow')
      } else {
        downloadFile <- tempfile()
        download.file(private$..korpus$meta$url, destfile = downloadFile,
                      mode = 'wb')
        unzip(zipfile = downloadFile, overwrite = FALSE,
              exdir = fp, junkpaths = TRUE, files = private$..korpus$meta$files)
        unlink(downloadFile)
      }
    }, # End of public corpus methods

    #---------------------------------------------------------------------------#
    #                         Public Environment Methods                        #
    #---------------------------------------------------------------------------#
    createEnv = function(environment) {

      if (!self$existsEnv(environment$name)) {

        # Validate parameters
        assertive.types::is_list(environment)
        assertive.properties::is_non_empty(environment$name)
        assertive.properties::is_non_empty(environment$desc)
        assertive.types::assert_is_character(environment$name)
        assertive.types::assert_is_character(environment$desc)
        assertive.files::is_dir(environment$path)

        # Format environment
        environment$created = Sys.time()
        newEnv = rbindlist(list(environment))

        # Update environment data frame if it exists, otherwise create new
        if (nrow(private$..environments) != 0) {
          private$..environments = rbind(private$..environments, newEnv)
        } else {
          private$..environments = newEnv
        }
      } else {
        futile.logger::flog.warn(paste(environment$name,"already exists"),
                                 name = 'yellow')
      }
      invisible(self)
    },

    existsEnv = function(envName) {

      if (nrow(private$..environments) == 0) {
        return(FALSE)
      } else if (nrow(subset(private$..environments,
                    private$..environments$name == envName)) == 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },

    printEnv = function() {
      private$shared$currentEnv
    },

    archiveEnv = function(environment) {

      if (self$existsEnv(environment) == TRUE) {

        # Create archive environment from named environment
        e <- subset(private$..environments, private$..environments$name == environment)
        archive <- e
        archive$name = paste0(sub('\\..*', '', paste0(environment)),
                              format(Sys.time(),'_%Y%m%d_%H%M%S'))

        archive$desc = paste(sub('\\..*', '', paste0('Archived ',e$desc)),
                             format(Sys.time(),'%Y%m%d_%H%M%S'))
        archive$path = file.path('./archive', archive$name)
        self$createEnv(archive)

        # Copy data to archive
        base::dir.create(archive$path, recursive = TRUE)
        base::file.copy(e$path, archive$path, recursive=TRUE)

      } else {
        futile.logger::flog.warn(paste(name,"is not a valid environment"),
                                 name = 'yellow')
      }
    }
  ), # End fo public environment methods
  lock_objects = FALSE

  # readCorpus = function(what) {
  #
  #   # Validate input
  #   if (sum(unlist(lapply(docTypes, function(t) grepl(what, t)))) == 0) {
  #     futile.logger::flog.error(paste(what,"is not a valid document type"),
  #                              name = 'red')
  #   }
  # }
)

# raw <- list(
#   meta = list(
#     name = 'raw',
#     desc = 'Raw Corpus',
#     path = 'raw',
#     url = 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
#     files = c(file.path('final/en_US/en_US.blogs.txt'),
#               file.path('final/en_US/en_US.news.txt'),
#               file.path('final/en_US/en_US.twitter.txt'))
#   ),
#   registers = registers
# )
#
# dev <-list(
#   name = 'dev',
#   desc = 'Development Environment',
#   path = './tests/data'
# )
# rawCorpus <- Korpus$new(raw)
# rawCorpus$strCorpus()
# rawCorpus$createEnv(dev)
# rawCorpus$printEnv()
# rawCorpus$currentEnv <- dev
# rawCorpus$read('registers')

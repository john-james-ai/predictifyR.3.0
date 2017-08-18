Korpus = R6::R6Class("Korpus",
  private = list(
    ..platform = character(0),
    ..currentEnv = data.frame(row.names = FALSE),
    ..korpusName = character(0),
    ..korpusDesc = character(0),
    ..korpusPath = character(0),
    ..korpusRegisters = list(),
    ..korpusRegisterDf = data.frame(),

    validateKorpusInput = function(platform, korpusName, korpusDesc = NULL) {

      # Supress automatically generated error messages
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))

      # Validation all is string
      assertive.types::assert_is_a_string(platform)
      assertive.types::assert_is_a_string(korpusName)

      # Validate non empty string
      if (korpusName == "" ) {
        futile.logger::flog.error(paste(
          "Error in initialize Korpus.",
          "Corpus name must not contain empty string"), name = 'red')
        stop()

      } else {
        futile.logger::flog.info(paste(
          "Platform is", platform), name = 'green')

        # Validate platform
        p <- mget(platform, inherits = TRUE,
                  ifnotfound = list('Platform not found'))

        if (class(p[[1]]) == "character") {
          futile.logger::flog.error(
            "Error in initialize Korpus. Invalid platform", name = 'red')
          stop()

        } else {

          # Validate corpus
          k <- mget(korpusName, inherits = TRUE, ifnotfound = list('Corpus not found'))
          if (class(k) == "Korpus") {
            futile.logger::flog.error(paste(
              "Error in initialize Korpus.", "Corpus", korpusName, "already exists!"), name = 'red')
            stop()
          }
        }
      }
    }
  ),

  active = list(
    platform = function() { private$..platform },
    currentEnv = function() { private$..currentEnv },
    korpusName = function() { private$..korpusName },
    korpusDesc = function() { private$..korpusDesc },
    korpusPath = function() { private$..korpusPath },
    korpusRegisters = function() { private$..korpusRegisters },
    korpusRegistersDf = function() { private$..korpusRegistersDf }
  ),

  public = list(

    initialize = function(platform, korpusName, korpusDesc) {

      private$validateKorpusInput(platform, korpusName, korpusDesc)

      if (is.na(korpusDesc) | is.null(korpusDesc) | korpusDesc == "" | !is.character(korpusDesc)) {
        korpusDesc <- korpusName
      }

      # Instantiate
      private$..platform = platform
      private$..korpusName = korpusName
      private$..korpusDesc = korpusDesc
      private$..korpusPath = tolower(gsub(pattern = " ",
                                          replacement = "-", korpusName))
      invisible(self)
    },

    addRegister = function(registerName, registerDesc, registerFileName, registerObjName) {

      private$validateRegisterInput(platform, korpusName, korpusDesc)

      newRegister = list()
      names(newRegister) <- registerName
      newRegister$registerName = registerName
      newRegister$registerDesc = registerDesc
      newRegister$registerFileName = registerFileName
      newRegister$registerObjName = registerObjName
      r <- Register$new(newRegister)
      private$..registers <- c(private$..registers, r)
      df <- data.frame(Name = registerName,
                       Desc = registerDesc,
                       Path = r$path,
                       FileName = registerFileName,
                       ObjName = registerObjName)

      if (nrow(private$..register) == 0) {
        private$..registers <- df
      } else {
        private$..register <- rbind(private$..registers, df)
      }
    }

  )
)

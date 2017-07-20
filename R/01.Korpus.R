Korpus = R6::R6Class("Korpus",
  private = list(
    ..platform = character(0),
    ..currentEnv = data.frame(),
    ..korpusName = character(0),
    ..korpusDesc = character(0),
    ..korpusPath = character(0),
    ..korpusRegisters = list(),
    ..korpusRegisterDf = data.frame()
  ),

  public = list(

    initialize = function(platform, korpusName, korpusDesc, korpusPath) {

      # Validation
      assertive.types::is_character(korpusName)
      assertive.types::is_character(korpusDesc)
      assertive.types::is_character(korpusPath)
      assertive.strings::is_a_non_missing_nor_empty_string(korpusName)
      assertive.strings::is_a_non_missing_nor_empty_string(korpusDesc)
      assertive.strings::is_a_non_missing_nor_empty_string(korpusPath)

      if (korpusName == "" | korpusDesc == "" | korpusPath == "") {
        futile.logger::flog.error(paste(
          "Error in initialize Korpus.", "Parameters must not contain empty string"), name = 'red')
      } else {

        p <- mget(platform, inherits = TRUE, ifnotfound = list('Platform not found'))
        if (length(p[[1]]) == 1) {
          futile.logger::flog.error(
            "Error in initialize Korpus. Invalid platform", name = 'red')

        } else {

          k <- mget(korpusName, inherits = TRUE, ifnotfound = list('Corpus not found'))
          if (length(k[[1]]) > 1) {
            futile.logger::flog.error(paste(
              "Error in initialize Korpus.", "Corpus", name, "already exists!"), name = 'red')

          } else {

          # Instantiate
          private$..platform = platform
          private$..korpusName = korpusName
          private$..korpusDesc = korpusDesc
          private$..korpusPath = korpusPath
          invisible(self)
          }
        }
      }
    },

    addRegister = function(registerName, registerDesc, registerFileName, registerObjName) {

      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))

      # Validate
      assertive.types::is_character(registerName)
      assertive.types::is_character(registerDesc)
      assertive.types::is_character(registerFileName)
      assertive.types::is_character(registerObjName)
      assertive.strings::is_a_non_missing_nor_empty_string(registerName)
      assertive.strings::is_a_non_missing_nor_empty_string(registerDesc)
      assertive.strings::is_a_non_missing_nor_empty_string(registerFileName)
      assertive.strings::is_a_non_missing_nor_empty_string(registerObjName)

      if (nrow(private$..korpusRegisterDf) != 0) {
        if (nrow(subset(private$..korpusRegisterDf,
                        private$..korpusRegisterDf$Name == registerName)) != 0) {
          futile.logger::flog.error(paste(
            "Error in initialize Korpus.", "Register", registerName,
            "already exists!"), name = 'red')
          stop()
        } else if (nrow(
          subset(private$..korpusRegisterDf,
                 private$..korpusRegisterDf$Desc == registerDesc)) != 0) {
          futile.logger::flog.error(paste(
            "Error in initialize Korpus.", "Register by that description",
            "already exists!"), name = 'red')
          stop()
        } else if (nrow(
          subset(private$..korpusRegisterDf,
                 private$..korpusRegisterDf$FileName == registerFileName)) != 0) {
          futile.logger::flog.error(paste(
            "Error in initialize Korpus.", "Register by that file name",
            "already exists!"), name = 'red')
        } else if (nrow(
          subset(private$..korpusRegisterDf,
                 private$..korpusRegisterDf$ObjName == registerObjName)) != 0) {
          futile.logger::flog.error(paste(
            "Error in initialize Korpus.", "Register by that object name",
            "already exists!"), name = 'red')
        } else {

        }
      } else {

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
      }
  )
)

# rm(raw)
# raw <- Korpus$new("predictifyR", 'raw', 'Raw Corpus', 'raw')


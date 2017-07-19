Document = R6::R6Class("Document",
  private = list(
    ..environment = character(0),
    ..korpus = R6::R6Class(korpus),
    ..type = character(0), # c('text', 'posTags', 'nGrams')
    ..name = character(0),
    ..path = character(0),
    ..desc = character(0),
    ..fileName = character(0),
    ..objName = character(0),
    shared = {
      e <- new.env()
      e$currentEnv <- NULL
      e
    }
  ),

  public = list(
    initialize = function(env, korpus, type, path, name, path, desc, fileName, objName) {

      # Validate Required Parameters

      parent <- get(korpus, inherits = TRUE)
      if (!("Korpus" %in% class(parent))) {
        futile.logger::flog.error(paste("Error in Document initialize method",
                                        korpus, "corpus not found"),
                                  name = 'red')
      }
      assertive.types::




    }

  )
)

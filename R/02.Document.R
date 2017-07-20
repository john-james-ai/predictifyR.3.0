Document = R6::R6Class(
  "Document",
  private = list(
    ..platform = character(0),
    ..korpus = character(0),
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..fileName = character(0),
    ..objName = character(0),
    ..created = character(0),
    shared = {
      e <- new.env()
      e$currentEnv <- NULL
      e
    },
    getEnvPath = function() {
      p <- get(platform, inherits = TRUE)
      env <- p$currentEnv
      env$Path
    },

    getKorpus = function() {
      k <- get(korpus, inherits = TRUE)

    }
  )
)

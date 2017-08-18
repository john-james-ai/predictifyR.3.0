.onLoad <- function(libname, pkgname) {

  # Establish home environment
  .pkgGlobalEnv <- new.env(parent = emptyenv())
  .pkgGlobalEnv$env <- data.frame(envName = "home",
                                  envDesc = "Home environment",
                                  envPath = file.path("./environments", "home"),
                                  envStatus = TRUE, envCreated <- Sys.time())
  .pkgGlobalEnv$paths <- list(
    environments = './environments',
    archives = './archives',
    logs = 'logs',
    corpora = list(
      raw = "raw",
      clean = "clean",
      processed = "processed",
      training = "training",
      validation = "validation",
      test = "test"
    ),
    documents = list(
      registers = "registers",
      posTags = "posTags",
      posPairs = "posPairs",
      nGrams = "nGrams"
    )
  )
  .pkgGlobalEnv
}

Corpus = R6Class("Corpus",

  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..created = NULL
  ),

  public = list(
    initialize = function(platform, name, desc,path, registers) {

      # Assert parameters
      assertive.properties::is_non_empty(platform)
      assertive.properties::is_non_empty(name)
      assertive.properties::is_non_empty(desc)
      assertive.types::assert_is_character(name)
      assertive.types::assert_is_character(desc)
      assertive.files::is_dir(path)
      assertive.types::is_list(registers)

      # Instantiate
      private$..platform = platform
      private$..name = name
      private$..desc = desc
      private$..path = path
      private$..created = Sys.time()
      private$..registers = registers
    }
  ),

  readCorpus = function(what) {

    # Validate input


  }
)

rawCorpus <- Corpus$new(predictify, 'raw', 'Raw Corpus', 'raw', registers)

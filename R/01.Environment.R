Environment <- R6::R6Class ("Environment",
 private = list(
   ..name = character(0),
   ..desc = character(0),
   ..path = character(0),
   ..created = character(0)
 ),
 public = list(

   initialize = function(name, desc, path) {

     # Validate parameters
     assertive.properties::is_non_empty(name)
     assertive.properties::is_non_empty(desc)
     assertive.properties::is_non_empty(path)
     assertive.types::assert_is_character(name)
     assertive.types::assert_is_character(desc)
     assertive.types::assert_is_character(path)

     # Initialize
     private$..name = name
     private$..desc = desc
     private$..path = path
     private$..created = Sys.time()

   },

 lock_objects = FALSE
 )
)

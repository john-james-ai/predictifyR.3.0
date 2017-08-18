## ---- parallelize_task
#==============================================================================#
#                             parallelizeTask                                  #
#==============================================================================#
#' parallelizeTask
#'
#' \code{parallelizeTask} parallelize task
#'
#' This generic function used to parallelize a task (when possible)
#'
#' @param task - the function to parallelize
#' @param ... - other parameters required by task
#' @return r - return from task
#' @author John James, \email{jjames@@datasciencestudio.org}
#' @export
parallelizeTask <- function(task, ...) {

  # Calculate number of cores
  ncores <- parallel::detectCores() - 1

  # Initiate cluster
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)

  # Run Task
  r <- task(...)

  # Stop cluster
  parallel::stopCluster(cl)

  return(r)
}
## ---- end

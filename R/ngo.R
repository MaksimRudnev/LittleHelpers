#' @name andgo
#' @rdname andgo
#'
#' @title Save'n'go & Append'n'go
#'
#'@description  Two ridiculously  simple but very useful functions.
#' 'savengo'  saves objects from a middle of your pipe and passes the same object to further elements of the pipe. It allows more efficient debugging and less confusing code, in which you don't have to interrupt your pipe every time you need to save an output.
#' Its sister function 'appendngo' appends an intermediary product to an existing list or a vector.
#' By analogy, one can create whatever storing function they need.
#'
#' @examples
#' # Example 1
#' #Saves intermediary result as an object called intermediate.result
#
#' final.result <- dt %>% dplyr::filter(score<.5) %>%
#'    savengo("intermediate.result") %>%
#'    dplyr::filter(estimated<0)
#'
#' # Example 2
#' #Saves intermediary result as a first element of existing list myExistingList
#'
#' final.result <- dt %>% dplyr::filter(score<.5) %>%
#'       appendngo(myExistingList, after=0) %>%
#'       dplyr::filter(estimated<0)
#'
#'@md
#'
#'
NULL

#' @rdname andgo
#'
#' @export
savengo <- function(object) {

  assign(name, object, envir=.GlobalEnv)

  object

}

#' @rdname andgo
#'
#' @export
appendngo <- function(object, what, after) {

  append(what, object, after=after)

  object

}

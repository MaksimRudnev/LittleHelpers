#' @name ramification
#' @rdname ramification
#'
#' @title Branching pipes
#'
#'@description Three little functions that allow for branching pipes. It is against Hadley's idea, as pipes are in principle linear, and in general I agree, but sometimes it would be comfy to ramify pipes away. It overcomes native *magrittr*'s *\%T>\%* by allowing more than one step after cutting the pipe.
#'
#' @param . Usually omitted.
#' @param ram.id If only two branches are needed, can be omitted. Otherwise id of the branch.
#' @param branch.id Equals 2 by default. Family of branches, or parent branch.
#' @param clear Logical, TRUE by default. Clear the buffer (delete hidden object from Global Environment).
#'
#' @details Imagine you need to create a list with means, correlations, and regression results. And you like to do it in one single pipe. In general, it is not possible, and you'll have to start a second pipe, probably doing some redundant computations.
#'
#'  \emph{"Pipes are fundamentally linear and expressing complex relationships with them will typically yield confusing code."}
#'  \url{http://r4ds.had.co.nz/pipes.html#when-not-to-use-the-pipe}
#'
#'  \describe{
#'   \item{ramify}{Saves current result into temporary object `.buf` and identifies a point in the pipe where branching will happen. Argument is an id of a ramification.}
#'   \item{branch}{Starts a new branch from the `ramify` point. (`branch(1)` can be omitted, as `ramify` creates the first branch. Second argument is a family of branches, or parent branch. By default it uses the last parent branch created by the last used `ramify`.}
#'   \item{harvest}{Returns contents of all the branches as a list.}
#'   \item{getbranch}{to access information from neghbouring branches. Not implemented yet.}
#' }
#'  \if{html}{\figure{branch.png}{options: width=400 alt="Ramification function"}}
#'
#'
#' @examples
#' data.frame(a=1:5, b=1/(1+exp(6:10)) ) %>%
#'   ramify(1) %>%
#'   branch(1) %>% colMeans %>%
#'     branch(2) %>% lm(a ~ b, .) %>% broom::tidy(.) %>%
#'     branch(3) %>% cor %>%
#'   ramify(2) %>%
#'     branch(1) %>% round(2) %>%
#'     branch(2) %>% psych::fisherz(.) %>%
#'   harvest(2) %>%
#' harvest
#'
#'
#' @md
#'
#'
NULL

#' @rdname ramification
#'
#' @export
ramify <- function(., ram.id=1) {
  #branch.id = 1
  if(ram.id==1) {
    a <- list()
  } else {
    a <- get(".buf", envir=.GlobalEnv )
  }

  a[[ram.id]] <- list()
  a[[ram.id]][["buffer.ramify"]]<-.

  assign(".buf", a, .GlobalEnv)

  return(.)
}

#' @rdname ramification
#'
#' @export
branch <- function(., branch.id=2, ram.id=NULL) {

  buf<- get(".buf", envir=.GlobalEnv )

  # Choose the last ram.id if it is not specified
  if(is.null(ram.id)) ram.id=length(buf)

  # Save current contents of buffer
  buffer.ram<-buf[[ram.id]][["buffer.ramify"]]

  # If it's not a first branch, append the corresponding stack of the buffer
  if(branch.id!=1) {
    buf[[ram.id]][[branch.id]]<-.
    assign(".buf", buf, .GlobalEnv)
  }

  return(buffer.ram)
}

#' @rdname ramification
#'
#' @export
harvest <- function(., ram.id=1, clear=TRUE) {
  a.new <- get(".buf", envir=.GlobalEnv )
  a.new[[ram.id]][[length(a.new[[ram.id]])+1]]<-.
  if(ram.id==1) clear=TRUE else clear=FALSE
  if(exists(".buf", envir=.GlobalEnv) & clear)  rm(.buf, envir=.GlobalEnv)
  a.new[[ram.id]][-1]

}


#'
#' @name andgo
#' @rdname andgo
#'
#' @title Save'n'go & Append'n'go
#'
#' @description  Two ridiculously  simple but very useful functions.
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

#'@rdname andgo
#'
#'@export
savengo <- function(object) {

  assign(name, object, envir=.GlobalEnv)

  object

}

#'@rdname andgo
#'
#'@export
appendngo <- function(object, what, after) {

  append(what, object, after=after)

  object

}

#' @name ramification
#' @rdname ramification
#'
#' @title Branching pipes
#'
#'@description Three little functions that allow for brunching pipes. It is against Hadley's idea, as pipes are in principle linear, and in general I agree, but sometimes it would be comfy to ramify pipes away. It overcomes native 'magrittr' `%T>%`` by allowing more than one step after cutting the pipe.
#'
#' @param . Usually omitted.
#' @param ram.id If only two branches are needed, can be omitted. Otherwise id of the branch.
#' @param branch.id Equals 2 buy default. Family of branches, or parent branch.
#' @param clear Logical, TRUE by default.
#'
#' @details
#'Imagine you need to create a list with means, correlations, and regression results. And you like to do it in one single pipe. In general, it is not possible, and you'll have to start a second pipe, probably doing some redundant computations.
#'
#'  \emph{"Pipes are fundamentally linear and expressing complex relationships with them will typically yield confusing code."}
#'  \url{http://r4ds.had.co.nz/pipes.html#when-not-to-use-the-pipe}
#'
#'  \describe{
#'   \item{ramify}{Saves current result into temporary object `.buf` and identifies a point in the pipe where branching will happen. Argument is an id of a ramification.}
#'   \item{branch}{Starts a new brunch from the `ramify` point. (brunch(1) can be omitted, as ramify creates the first brunch. Second argument is a family of branches, or parent branch. By default it uses the last parent branch created by the last used `ramify`.}
#'   \item{harvest}{Returns contents of all the brunches as a list.}
#'   \item{getbranch}{to access information from neghbouring branches. Not implemented yet.}
#' }
#'  \if{html}{\figure{branch.png}{options: width=400 alt="Ramification function"}}
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

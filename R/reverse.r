#' Reverse values and value labels
#'
#' I don't know why it wasn't developed before. Returns same variable with reversed values.
#'
#' Reversing makes use of observed values rather than stored factor levels or attributes.
#' @param var variable to reverse.
#' @export
reverse <-function(var) {
  straight<-sort(unique(var), F)
  reversed<-sort(unique(var), T)
  new.var<-sapply(var, function(x) reversed[straight==x][1] )

  if(length(attr(var, "labels"))!=0) {
    old.labels <- attr(var, "labels")
    names(reversed) <- sapply(straight, function(x)  names(old.labels)[old.labels==x]  )
    attr(new.var, "labels")<-reversed
  }
  new.var
}


#reverse(ess8$netusoft)

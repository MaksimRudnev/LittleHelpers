#' Reverse values and value labels
#'
#' @description  Returns same variable with reversed values. Reversing makes use of observed values rather than stored factor levels or attributes.
#' @param var variable to reverse.
#' @param preserve.labels Attempt saving labels.
#' @export
reverse <-function(var, preserve.labels = T) {

  straight<-sort(unique(var), F)
  reversed<-sort(unique(var), T)

  #new.var<-sapply(var, function(x) reversed[straight==x][1] )

  # Faster version
  new.var <- rep(NA, length(var))
  for(i in 1:length(straight) ) new.var[var==straight[i]] <- reversed[i]


  if(length(attr(var, "labels", exact = T))!=0 & preserve.labels) {
    old.labels <- attr(var, "labels", exact = T)
    names(reversed) <- sapply(straight, function(x)  names(old.labels)[old.labels==x]  )
    attr(new.var, "labels") <- reversed
    attr(new.var, "label") <- attr(var, "label", exact = T)
  }
  new.var
}


#reverse(ess8$netusoft)

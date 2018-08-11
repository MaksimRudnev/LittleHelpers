#' Correlation table with stars
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param d Data.frame
#' @param method Method from `cor` function. spearman by default.
<<<<<<< HEAD
#' @param star If stars for p levels should be added.
=======
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
#'
#' @export
cor_table <- function(d, method="spearman", star=TRUE) {

  if(any(class(d)=="tbl"))  d<- drop_labs(untibble(d))

  #print(str(d))

  r <-  cor(d, use="pairwise.complete.obs", method=method)

  n <- sapply(1:ncol(d), function(a) {
    sapply(1:ncol(d), function(b) {
      v<-!is.na(d[,c(a,b)]) %>% rowSums(.)==2
      sum(v)
    })})

  t <-  r / (sqrt((1-r^2)/(n-2)))

<<<<<<< HEAD
  p <- 2*stats::pt(abs(t), n-2, lower=FALSE)
=======
  p <- 2*pt(abs(t), n-2, lower=FALSE)
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5

  starz <- matrix(rep("", ncol(d)^2), nrow=ncol(d))
  starz[p<0.05]<-"*"
  starz[p<0.01]<-"**"
  starz[p<0.001]<-"***"


  if(star) {
    m<- paste(format(round(r, 2), digits=2, nsmall = 2), starz, sep="")
    m<-as.data.frame(matrix(m, nrow=ncol(d)))
  } else {
    m<- paste(r, sep="")
    m <- as.data.frame(matrix(m, nrow=ncol(d)))
  }

  names(m)<-names(d)
  rownames(m)<-names(d)
  m
}
<<<<<<< HEAD

#' Rename variables in a data.frame
#'
#' @param old.names Old name(s) existing in the data. A vector or a single character value.
#' @param new.names New name(s). A vector of chatacter names in a same order as 'old.names'.
#' @param data A data frame.
#'
#' @export
rename <- function(old.names, new.names, data) {
  if(any(!old.names %in% names(data))  ) {

    stop(paste("Can't find variable names:", paste(old.names[!old.names %in% names(data)], collapse="; "),
               "\n  Nothing has been renamed"))
  } else {
    names(data)[names(data) %in% old.names]<-new.names
  }
  data
}
=======
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5

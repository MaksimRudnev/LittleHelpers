#' Correlation table with stars
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param d Data.frame
#' @param method Method from `cor` function. spearman by default.
#'
#' @export
cor_table <- function(d, method="spearman") {

  r <-  cor(d, use="pairwise", method=method)

  n <- sapply(1:ncol(d), function(a) {
    sapply(1:ncol(d), function(b) {
      v<-!is.na(d[,c(a,b)]) %>% rowSums(.)==2
      sum(v)
    })})

  t <-  r / (sqrt((1-r^2)/(n-2)))

  p <- 2*pt(abs(t), n-2, lower=FALSE)

  stars <- matrix(rep("", ncol(d)^2), nrow=ncol(d))
  stars[p<0.05]<-"*"
  stars[p<0.01]<-"**"
  stars[p<0.001]<-"***"



  m<- paste(format(round(r, 2), digits=2, nsmall = 2), stars, sep="")
  m<-as.data.frame(matrix(m, nrow=ncol(d)))
  names(m)<-names(d)
  rownames(m)<-names(d)
  m
}

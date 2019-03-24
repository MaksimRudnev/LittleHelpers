#' Correlation table with stars
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param d Data.frame
#' @param method Method from `cor` function. spearman by default.
#' @param star If stars for p levels should be added.
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

  p <- 2*stats::pt(abs(t), n-2, lower=FALSE)

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


#' Rename variables in a data.frame
#'
#' @param old.names Old name(s) existing in the data. A vector or a single character value.
#' @param new.names New name(s). A vector of chatacter names in a same order as 'old.names'.
#' @param data A data frame.
#'
#' @export
# rename <- function(old.names, new.names, data) {
#   if(any(!old.names %in% names(data))  ) {
#
#     stop(paste("Can't find variable names:", paste(old.names[!old.names %in% names(data)], collapse="; "),
#                "\n  Nothing has been renamed"))
#   } else {
#     names(data)[names(data) %in% old.names]<-new.names
#   }
#   data
# }

rename <- function(old.names, new.names, dat) {

  dat.names  = names(dat)

  if(any(!old.names %in% dat.names)  ) warning(paste(
    "Can't find variable names:", paste(old.names[!old.names %in% dat.names], collapse="; ")))


  for(x in 1:length(old.names))  {

    if(any(dat.names %in% old.names[x] )) dat.names[ dat.names == old.names[x] ] <- new.names[x]

  }

  dat.new = dat
  names(dat.new) <- dat.names
  return(dat.new)
  }



#'St err of mean
#'
#'@export
se <- function(variable) {
  variable <- stats::na.omit(variable)
  sqrt(stats::var(variable)/length(variable))

}



pvalue_to_stars <- function(vector.of.pvalues) {
  sapply(vector.of.pvalues, function(x) paste0(
  ifelse(x < .001, "***", ifelse(x < .01, "**", ifelse(x < .05, "*", " ")))))
}

# Match named vectors to a data.frame
#'@param ... Named vectors with same of partly different names. OR a list of named vectors.
#'
#' @return The function returns a data.frame with vectors stacked as columns.
#'
#'@export
match_named_vectors <- function(...) {
  l <- list(...)

  if(length(l)==1) l <- l[[1]]

  all.nmz <-  unlist(sapply(l, names))
  all.nmz <-all.nmz[!duplicated(all.nmz)]
  sapply(l, function(x) unlist(x)[all.nmz])

}





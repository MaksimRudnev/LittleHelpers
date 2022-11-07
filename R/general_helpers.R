#' Correlation table with stars
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param d Data.frame
#' @param method Method from `cor` function. "spearman" by default.
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
  as.matrix(m)
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



#' Match named vectors to a data.frame
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
  out <- sapply(l, function(x) unlist(x)[all.nmz])
  rownames(out)<- all.nmz
  return(out)
}




#' Prints the number and percent of NAs in a dataframe
#'@param data Data.frame
#'@param viewer If `TRUE``, show in Rstudio viewer, if FALSE returns a data.frame
#'
#'
#'
#'@export
na_tab <- function(data, viewer = T) {
  lst <- apply(data, 2, function(x) data.frame(NA_total=sum(is.na(x)),  NA_percent = scales::percent(sum(is.na(x))/length(x))  ))
  nms <- names(lst)
  lst <-  Reduce("rbind", lst)
  rownames(lst)<-nms
  if(viewer) df_to_viewer(lst) else return(lst)
}



#' Easy cross-tabulation with labels
#' @param rows Character, variable name to put in rows
#' @param cols Character, variable name to put in columns
#' @param data Data.frame containing variables
#' @param margin If any proportions should be computed, might be `row`, `col`, or `none`.
#' @param useNA How to deal with NAs, passed to `table`.
#' @param drop.empty Remove empty categories?
#' @examples
#'\dontrun{
#'    crosstab("country", "frequency", wvs6, "row")
#' }
#'

#'
#' @export
crosstab <- function(rows, cols, data, margin="row", useNA="always", drop.empty = T) {

  row = lab_to_fac(data[,rows])
  col = lab_to_fac(data[,cols])

  if(drop.empty) {
    if(is.factor(row)) row <- droplevels(row)
    if(is.factor(col)) col <- droplevels(col)
  }


  tb <- table(row,col, useNA=useNA)

  if(margin=="col") {
    tb <- prop.table(tb, 2)
  } else if (margin == "row") {
    tb <- prop.table(tb, 1)
  }

  rownames(tb)[is.na(rownames(tb))]<-".NA"


  if("tbl_df" %in% class(data)) {
    title = paste("[", rows, "]",  attr(lab_to_fac(data[,rows]), "header"), " BY ",
                  "[", cols, "]", attr(lab_to_fac(data[,cols]), "header"), sep="")

  } else {
    title = paste("[", rows, "]",  "[", cols, "]")
  }

  print(title)
  print(tb)

    df_to_viewer(
      as.data.frame.matrix(tb),
       digits=2,
       title=title)


}


#' Format the number quickly and remove zero before dot
#' @param x any numeric vector or value.
#' @param digits number of decimals.
#' @returns Character of the formatted
#' @export
f = function(x, digits=3) {
  ifelse(grepl("^-", x),
         sub("^-0\\.", "-.", sprintf(paste0("%.", digits, "f"), x)),
         sub("^0\\.", ".", sprintf(paste0("%.", digits, "f"), x))
  )
}

#' Facilitates setting up a code and running measurement invariance test in Mplus
#'
#' @param model Model in Mplus format, e.g. F1 BY one two three;
#' @param group Character,  group variable name
#' @param data Data
#' @param Mplus_com A system call to Mplus, usually just "mplus" (default)
#' @details Creates three files in the working directory: mplus_temp.tab, mplus_temp.inp, and mplus_temp.out. The output is stored in the latter.
#' @examples
#' # measurementInvarianceMplus(model = "F1 BY F114 F115 F116 F117;
#' #                                     F2 BY F118 F119  F120  F121 F122  F123;",
#' # group = "S003",
#' # data = wevs.subset,
#' # Mplus_com = "wine mplus"
#' # )




measurementInvarianceMplus <- function(model,
                                       group,
                                       data,
                                       Mplus_com = "mplus") {
  model <- gsub(" +|\t+", " ", model)[[1]]
  var.list <- strsplit(model, ";|\n") [[1]]
  var.list <-   var.list[!var.list==""]
  var.list <-   unlist(strsplit(var.list, "(?i)(by)", perl=TRUE))
  var.list <-   unlist(strsplit(var.list[seq(2, length(var.list), by=2)], " "))
  var.list <- paste(unique(unlist(var.list)), collapse=" ")
  var.list <- strsplit(var.list, " ")[[1]]
  var.list <-   var.list[!var.list==""]

  d <- data[c(group, var.list)]
  for(i in colnames(d)) d[,i] <- unclass(d[,i])
  rm(i)
  class(d)<-"data.frame"
  if(!is.numeric(d[,group])) {
    #d[,group] <- gsub(" ", "_", as.character( d[,group] )  )
    warning("The group variable must be numeric!")


  }

  write.table(d, "mplus_temp.tab", quote=F, sep="\t", row.names=F, col.names=F, na=".")

  cat(paste("
DATA: file is mplus_temp.tab;
VARIABLE:
  NAMES = ", group, paste(var.list, collapse = "\n          "),
            ";\n  MISSING=.;
GROUPING IS",  group, " (", paste(unique(d[,group]), collapse = "\n            "), ");\n",
            'ANALYSIS:
  estimator = MLR;
  model= configural metric scalar;
  ITERATIONS = 100000;
MODEL:\n', model),

      file = "mplus_temp.inp")

  message("Run free in Mplus.")
  trash <- system(paste(Mplus_com, "mplus_temp.inp"))

}

#' Shows data.frame in RStudio viewer
#'
#' via stargazer
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param ... Other arguments passed to stargazer.
#' @param summ Passed to `summary` argument of stargazer.
#'
#' @aliases to_viewer
#' @export

df_to_viewer <- function(df, rownames = TRUE, summ=F,...) {
  library(stargazer, quietly=T)
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  a<-capture.output(stargazer(df, summary=summ,
                                         #out=htmlFile,
                                         type="html",
                                         rownames = rownames, style="ajs", ...))

  cat("<head>
  <meta charset='UTF-8'>
      </head>", file=htmlFile)
  cat(a, file=htmlFile, append=T)

  viewer <- getOption("viewer")
  viewer(htmlFile)
  #rm(this,b,viewer)
}

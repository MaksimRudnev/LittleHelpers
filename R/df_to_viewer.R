#' Shows data.frame in RStudio viewer
#'
#' via stargazer
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param ... Other arguments passed to stargazer.
#' @param summ Passed to `summary` argument of stargazer.
#' @param kable logical, if data.frame should be passed to kable. If true, ... will be passed to function kableExtra::kable_styling
#'
#' @aliases to_viewer
#' @export

df_to_viewer <- function(x, rownames = TRUE, summ=F, kable = FALSE, html = FALSE, ...) {
  if(!kable) {
      library(stargazer, quietly=T)
      tempDir <- tempfile()
      dir.create(tempDir)
      htmlFile <- file.path(tempDir, "index.html")

      if(!html) {
      a<-capture.output(stargazer(x, summary = summ,
                                             #out=htmlFile,
                                             type="html",
                                             rownames = rownames, style="ajs", ...))
      } else {
        a = x
      }
      cat("<head>
      <meta charset='UTF-8'>
          </head>", file=htmlFile)
      cat(a, file=htmlFile, append=T)

      viewer <- getOption("viewer")
      viewer(htmlFile)
      #rm(this,b,viewer)
  } else {
    knitr::kable(x, format  = "html") %>% kableExtra::kable_styling(...)
  }
}

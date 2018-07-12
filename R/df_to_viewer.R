#' Shows data.frame in RStudio viewer
#'
#' via stargazer
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param ... Other arguments passed to stargazer.
#' @export

df_to_viewer <- function(df, rownames = TRUE, ...) {

  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  a<-capture.output(stargazer::stargazer(df, summary=F,
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

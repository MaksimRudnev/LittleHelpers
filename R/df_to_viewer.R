#' Shows data.frame in RStudio viewer
#'
#' via stargazer
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param ... Other arguments passed to stargazer.
<<<<<<< HEAD
#'
=======
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
#' @export

df_to_viewer <- function(df, rownames = TRUE, ...) {

  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
<<<<<<< HEAD
  a<-capture.output(stargazer(df, summary=F,
=======
  a<-capture.output(stargazer::stargazer(df, summary=F,
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
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

#' Shows data.frame in RStudio viewer
#'
#' via stargazer
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param summ Passed to `summary` argument of stargazer.
#' @param kable logical, if data.frame should be passed to kable. If true, ... will be passed to function kableExtra::kable_styling
#' @param by The name of variable to group the rows by. Used only when `kable=TRUE`
#' @param html If viewer should be ignored and just html code returned (useful to embed in Rmarkdown outputs
#' @param colformat Formats each column using `sprintf`. If numeric, applies rounding only, if character, is passed to `sprintf` directly. Can be named to match x's column names. If not named, must have the same length as there are columns in x.
#' @param ... Other arguments passed to `stargazer` or `kableStyling()`.
#' @param style 'style' argument of `stargazer`.
#'
#' @aliases to_viewer
#' @export

df_to_viewer <- function(x, rownames = TRUE, summ=F, kable = FALSE, by = NULL, html = FALSE, colformat = NULL, style = "ajs", ...) {

  if(!is.null(colformat)) {
    #colformat <- c(NA, 1,1,0,1,1,1,1,2,0,3)
    warning("we're going to format the numbers")

    if(!is.null(names(colformat)))  {
      colformat <- colformat[colnames(x)]
      names(colformat) <- colnames(x)
      it = colnames(x)
    } else {
      it = 1:ncol(x)
    }

    x_ <- sapply(it, function(y) {
      if(is.character(x[,y]) | is.na(colformat[[y]]))
        x[,y]
      else
        switch(class(colformat),
               numeric= sprintf(paste0("%.", colformat[[y]], "f"), x[,y]),
               character= sprintf(colformat[[y]], x[,y])
        )

    })

    dimnames(x_) <- dimnames(x)
    x<-x_
  }


  if(!kable) {
      library(stargazer, quietly=T)
      tempDir <- tempfile()
      dir.create(tempDir)
      htmlFile <- file.path(tempDir, "index.html")

      if(!html) {
      a<-capture.output(stargazer(x, summary = summ,
                                             #out=htmlFile,
                                             type="html",
                                             rownames = rownames, style=style, ...))
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

    if(is.null(by)) {

    knitr::kable(x, format  = "html", rownames = rownames) %>%
        kableExtra::kable_styling(...)

    } else {

      #sort table by unique values of 'by' in the way they appear in the data
      x <- x[unlist(sapply(unique(x[,by]), function(i) which(x[,by] == i))),]

    # save kable code to use in the loop below
     tbl.out <- kable(x[,-which(names(x)==by)],
                      format  = "html",
                      row.names = rownames) %>%
       kableExtra::kable_styling(...)

     # add packs of the rows
      end.position = 0
      for(i in 1:length(unique(x[,by]))) {
        new.end.position = sum(x[,by] == unique(x[,by])[i]) + end.position
        tbl.out <-
          tbl.out %>%
          pack_rows(
            group_label = unique(x[,by])[i],
            start_row =  end.position + 1,
            end_row = new.end.position
          )
        end.position = new.end.position

      }
      print(tbl.out)

    }
  }
}

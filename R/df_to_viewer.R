#' Shows data.frame in RStudio viewer
#'
#' via stargazer and knitr
#'
#' @param df data.frame to show. Also works with a list of fitted lm() models.
#' @param rownames Logical, if rownames should be shown.
#' @param summ Passed to `summary` argument of stargazer.
#' @param kable logical, if data.frame should be passed to kable. If true, ... will be passed to function kableExtra::kable_styling
#' @param by The name of variable to group the rows by. Used only when `kable=TRUE`
#' @param html If viewer should be ignored and just html code returned (useful to embed in Rmarkdown outputs
#' @param colformat Formats each column using `sprintf`. If numeric, applies rounding only, if character, is passed to `sprintf` directly. Can be named to match x's column names. If not named, must have the same length as there are columns in x.
#' @param ... Other arguments passed to `stargazer` or `kableStyling()`.
#' @param sg.style 'style' argument of `stargazer`.
#' @param k.style Applies custom kableStyling style, might take two values "default", and "custom".
#' @param kable.options A list of arguments passed to `kable` function.
#' @param cells.by.merge The headers created by `by` argument can be a single cell spanning all the columns (TRUE) or the subheader can be put in the new line's first column (FALSE). The latter tables are easier to hande in, e.g. MS Word.
#' @param cells.by.indent Logical. Whether 'by' headers should have an indent.
#'
#' @aliases to_viewer
#' @export

df_to_viewer <- function(x, rownames = TRUE, summ=F, kable = FALSE, by = NULL, html = FALSE, colformat = NULL, sg.style = "ajs", k.style = "default", digits = 2, kable.options = list(), cells.by.merge = T, cells.by.indent = T, ...) {



  if( k.style =="default") {
    kableStyling.arg <- function(kabTab, ...) {
      list(
      kable_input = kabTab,
      full_width = T,
      html_font = "Times new roman",
      font_size = 12,
      bootstrap_options="none",
      htmltable_class = "lightable-classic-2",
      ...
      )}
    } else {
      kableStyling.arg <- function(kabTab, ...) list(kabTab, ...)
    }

  if(!is.null(colformat)) {
    #colformat <- c(NA, 1,1,0,1,1,1,1,2,0,3)
   # warning("we're going to format the numbers")

    if(!is.null(names(colformat)))  {
      colformat <- colformat[colnames(x)]
      names(colformat) <- colnames(x)
      col.nmz = colnames(x)
    } else {
      col.nmz = 1:ncol(x)
    }

    x.formatted <- sapply(col.nmz, function(y) {
      if(is.character(x[,y]) | is.na(colformat[[y]]))
        x[,y]
      else
        switch(class(colformat),
               numeric= f(x[,y], colformat[[y]]),
               character= sprintf(colformat[[y]], x[,y])
        )

    })



    dimnames(x.formatted) <- dimnames(x)

    for(i in colnames(x.formatted))
      x.formatted[,i][x.formatted[,i]=="NA"]<-NA

    x<-x.formatted
  }


  if(!kable) {
    requireNamespace("stargazer", quietly = T)
      tempDir <- tempfile()
      dir.create(tempDir)
      htmlFile <- file.path(tempDir, "index.html")

      if(!html) {
      a<-capture.output(stargazer::stargazer(x, summary = summ,
                                             #out=htmlFile,
                                             type="html",
                                             rownames = rownames, style=sg.style, digits=digits, ...))
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
    requireNamespace("knitr", quietly=T)
    requireNamespace("kableExtra", quietly=T)

    if(is.null(by)) {


    kabTab <- do.call(knitr::kable, append(kable.options,
                                             list(x = x,
                                                  format  = "html",
                                                  row.names = rownames,
                                                  digits=digits)))
    do.call(kableExtra::kable_styling, kableStyling.arg(kabTab, ...))


    } else {

       #if(cell.by = "merged") {
      #sort table by unique values of 'by' in the way they appear in the data
      x <- x[unlist(sapply(unique(x[,by]), function(i) which(x[,by] == i))),]

    # save kable code to use in the loop below
      kabTab <- do.call(knitr::kable,
                                     append(kable.options,
                                            list(x = x[,-which(colnames(x)==by)],
                                                format  = "html",
                                                row.names = rownames,
                                                digits=digits)))
       tbl.out <- do.call(kableExtra::kable_styling, kableStyling.arg(kabTab, ...))


     # add packs of the rows
      end.position = 0
      for(i in 1:length(unique(x[,by]))) {
        new.end.position = sum(x[,by] == unique(x[,by])[i]) + end.position
        tbl.out <-
          tbl.out %>%
          kableExtra::pack_rows(
            group_label = unique(x[,by])[i],
            start_row =  end.position + 1,
            end_row = new.end.position,
            hline_before = F,
            hline_after = ifelse(cells.by.merge, F, T),
            label_row_css = ifelse(cells.by.merge,  "border-top: 1px solid;", ""),
            colnum = ifelse(cells.by.merge, ncol(x), 1),
            indent = cells.by.indent

          )
        end.position = new.end.position

      }
       print(tbl.out)


       # } else if(cells.by = "split") {



      # } else {
       #  warning("'cells.by' argument can only be 'merged' or 'split'")
       #}
    }
  }
}

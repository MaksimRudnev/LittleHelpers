#' Create a codebook, table of labels to explore variables and values in your data
#'
#' @param df data.frame, usually an object obtained by reading SPSS datafile with have::read_sav or foreign::read.spss
#' @param max.vals integer, how many value labels per each variable shoud be listed in the table, default is 25
#' @param vars can be integer, character, or range of integers or characters. Variables indexes or names for getting subsets of label book.
#' @param view logical, whether the result should be shown in the RStudio viewer pane. Defualt is TRUE. If FALSE, html file named 'label_book_output.html' is saved in your working directory.
#'
#' @details For Haven and Foreign read spss files only. The only extra package required is `knitr`.
#' @examples ## Example
#'\dontrun{
#' ## 1.Read the data
#' #ess8<- haven::read_sav("ESS8e01.sav")
#'
#' ## 2. Use the function
#' #label_book(ess8, max.vals=11, vars=45:50)
#'}
#' @md
#' @export

label_book <- function(df, max.vals=25, vars="all", view=TRUE) {

  if(vars=="all") {
    rotator <- 1:length(df)
  } else if (length(vars)>1 | is.character(vars)) {
    rotator <- vars
  } else {
    rotator <- 1:vars
  }

  if(max.vals=="all") max.vals <- 1000000

  # Guess if the data was read by haven or foreign package
  dat.structure<- ifelse(is.null(attr(df, "variable.labels")), "haven", "foreign")

  message("The data seems to be read by ", dat.structure)

 if(dat.structure=="haven") {

  labs<- lapply(rotator, function(i) {

    #Extract labels if they exist
    var.name<-names(df[i])

    if(sum(class(df)=="tbl")>0) {
      variable <- df[,i][[1]]
    } else {
      variable <- df[,i]
    }

    var.lab<-attr(variable, "label")

    if(is.null(attr(variable, "labels"))) {
      val.labs <- ""
      names(val.labs)<-""

    } else {

      if(length(attr(variable, "labels"))>=max.vals) {
      val.labs<-c(attr(variable, "labels")[1:max.vals], '-----truncated-----'="")
      } else {
      val.labs<-attr(variable, "labels")
      }
    }

    #Combine labels and variable names to a data.frame
    onevarlabs <- data.frame(
      Variables =  c(var.name,rep("", length(val.labs)-1)),
      Variable.labels = c( ifelse(is.null(var.lab), "", var.lab), rep("", length(val.labs)-1)),
      Values = val.labs,
      Value.labels = names(val.labs),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    names(onevarlabs)<-c("Variables", "Variable labels", "Values", "Value labels")
    onevarlabs
  } )

 } else if(dat.structure=="foreign") {

    labs<- lapply(rotator, function(i) {


      #Extract labels if they exist

      var.lab<-attr(df, "variable.labels")[i]
      var.name<-names(var.lab)

      if(is.null(attr(df[,i], "value.labels"))| length(attr(df[,i], "value.labels"))==0   ) {
        val.labs <- ""
        names(val.labs)<-""

      } else {

        if(length(attr(df[,i], "value.labels"))>=max.vals) {
          val.labs<-c(sort(attr(df[,i], "value.labels"))[1:max.vals], '-----truncated-----'="")
        } else {
          val.labs<-sort(attr(df[,i], "value.labels"))
        }
      }

      #Combine labels and variable names to a data.frame
      onevarlabs <- data.frame(
        Variables =  c(var.name,rep("", length(val.labs)-1)),
        Variable.labels = c( ifelse(is.null(var.lab), "", var.lab), rep("", length(val.labs)-1)),
        Values = val.labs,
        Value.labels = names(val.labs),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      names(onevarlabs)<-c("Variables", "Variable labels", "Values", "Value labels")
      onevarlabs

    } )
 }

 labs2<-Reduce(function(a, b) rbind(a, b), labs)

 # Export the table
 tempDir <- tempfile()
 dir.create(tempDir)
 htmlFile <- file.path(tempDir, "index.html")

 require("knitr")
 x<- kable(labs2, format = "html")
 x1 <- paste( '<!DOCTYPE html>
               <head>
               <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
               </head>
               <body>
               <h1>Label Book for "', as.character(sys.call())[2], '"</h1>',
              gsub('<table>', '<table class="table table-condensed table-hover table-bordered">', x),
              sep=""
              )

 cat(x1, file=ifelse(view, htmlFile, "label_book_output.html"), sep="\n")

 if(view) {
    viewer <- getOption("viewer")
    viewer(htmlFile)

    } else {
    message("The output was saved in 'label_book_output.html' file in your working directory.")
   }

}


# for1<- foreign::read.spss("/Users/maksimrudnev/Dropbox/STAT/WORLD values survey/WV6_Data_spss_v_2014_04_28.sav",
#                     to.data.frame=TRUE,
#                     use.value.labels = FALSE)
# hav1<- haven::read_sav("/Users/maksimrudnev/Dropbox/STAT/WORLD values survey/WV6_Data_spss_v_2014_04_28.sav")
#
#
# label_book(hav1, max.vals=25, vars=6)
# label_book(for1, max.vals=25, vars=6)
#
# label_book(hav1, max.vals=10, vars=6:10)
# label_book(for1, max.vals=25, vars=6:10)
#
# label_book(hav1, max.vals=25, vars="V5")
# label_book(for1, max.vals=25, vars="V5")
#
# label_book(hav1, max.vals=2, vars="all")
# label_book(for1, max.vals=2, vars="all")
#
# label_book(hav1, max.vals=2, vars=c("V6", "V2"))
# label_book(for1, max.vals=2, vars=c("V6", "V2"))
#
# label_book(for1, max.vals=2, vars=c("V6", "V2"), F)
#
# df<-hav1
#
# str(for1[,1:2])
# str(for1[,6])



#' Easy cross-tabulation with labels
#' @param rows Character, variable name to put in rows
#' @param cols Character, variable name to put in columns
#' @param data Data.frame containing variables
#' @param margin If any proportions should be computed, might be `row`, `col`, or `none`.
#' @param useNA How to deal with NAs, passed to `table`.
#'
#' @examples
#'\dontrun{
#'    crosstab("country", "frequency", wvs6, "row")
#' }
#'

#'
#' @export
crosstab <- function(rows, cols, data, margin="row", useNA="always") {

  tb <- table(lab_to_fac(data[,rows]),
              lab_to_fac(data[,cols]),
              useNA=useNA)

  if(margin=="col") {
    tb <- prop.table(tb, 2)
  } else if (margin == "row") {
    tb <- prop.table(tb, 1)
  }

  rownames(tb)[is.na(rownames(tb))]<-".NA"

  as.data.frame.matrix(tb) %>%
    df_to_viewer(digits=2
                 ,
                 title=paste("[", rows, "]",  attr(lab_to_fac(data[,rows]), "header"), " BY ",
                             "[", cols, "]", attr(lab_to_fac(data[,cols]), "header"), sep="")
                 )

}

#' Convert labelled variable to a factor with corresponding values
#' @param var.labelled Labelled variable or tibble with a single variable
#'
#' @export
lab_to_fac <- function(var.labelled) {

  if(any(class(var.labelled)=="tbl")) {
    #convert from tibble
    var.labelled <- var.labelled[[1]]
  }

  if(is.null(attr(var.labelled, "labels"))) {
    return(var.labelled)
    message("No labels in variable. Return the same variable.")
  } else {
  labs<-names(attr(var.labelled, "labels"))[  na.omit(match(unique(var.labelled), attr(var.labelled, "labels"))) ]

  labs<-labs[na.omit(match(names(attr(var.labelled, "labels")), labs))]
   out<-factor(var.labelled, labels=labs)
}
  attr(out, "header") <- attr(var.labelled, "label")
  out

}

#' Get rid of the labels
#'
#' @param dataframe Data.frame
#'
#' @export
drop_labs <- function(dataframe) {

  new.data <- dataframe

  for(i in names(dataframe)) {
    if(any(class(dataframe[,i])=="labelled"))  {
      new.data[,i] <- unclass(new.data[,i])
      attr(new.data[,i], "labels")<-NULL
    }}

  if(!is.data.frame(new.data)) {
    new.data <- unclass(dataframe)
    attr(new.data, "labels")<-NULL
  }

  new.data
}

#' Get rid of the tibble
#'
#' @export
untibble <- function(dataframe) {

  if("tbl" %in%  class(dataframe)) {
    as.data.frame(unclass(dataframe))

  } else {
    message("Dataframe is NOT a tibble. Return the same object.")
    dataframe
  }

}


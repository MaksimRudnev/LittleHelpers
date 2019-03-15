#' Replace by table
#'
#' Use a codes table to recode variable. Might be useful for translation of labels, for example.
#'
#' @param variable Variable to recode.
#' @param lookup.table Data frame with (at least) two columns named in line with the following arguments: `matching.column` and `replacement.column`
#' @param matching.column Character. Lookup table's column of containing (pattern) values simiar to ones in `variable`
#' @param replacement.column Character. Lookup table's column containing substitution values.
#' @param save.levels.order Logical. If the level order should follow the matching.column's levels order.
#'
#' @examples
#' library(readxl)
#' codes <- read_excel("countryCodes.xls", 1)
#' replace_by_table(mydata$var1, codes, "iso2c", "full.country.names")
#'
#'@export
replace_by_table<- function(variable=all.data$cntry, lookup.table=recodes2,
                            matching.column="cntry",
                            replacement.column="WDIcode", save.levels.order=T) {


  #NB rewrite using switch()


  # make data.frame, remove duplicates in matching.column with the FIRST match
  if("tbl" %in% class(lookup.table)) lookup.table<-untibble(lookup.table)

  lool <- lookup.table[, c(matching.column, replacement.column)]
  lool <- lool[ !duplicated(lool[,matching.column]), ]

  if(is.factor(lool[,matching.column]))    lool[,matching.column] <-    as.character(lool[,matching.column])
  if(is.factor(lool[,replacement.column])) lool[,replacement.column] <- as.character(lool[,replacement.column])

  # This method is 7 times faster, but doesnt work always
  # t<- Sys.time()
  # variable.new <- merge(x=data.frame(var=as.character(variable), stringsAsFactors = F ),
  #                       y=lool,
  #                       by.x="var",
  #                       by.y=matching.column,
  #                       all.x=T, sort=F )
  # print(Sys.time() - t)

  # This method is slower, but works
  #  t<- Sys.time()
  #  variable.new <- sapply(1:length(variable), function(vrow) {
  #
  #   if(any(lool[,matching.column]==variable[vrow]) | is.na(variable[vrow]) ) {
  #
  #   lool[,replacement.column][lool[,matching.column]==variable[vrow]]
  #   } else {
  #     variable[vrow]
  #   }
  # })
  # print(Sys.time() - t)







  #Faster method
  #t<- Sys.time()

  variable.new <- as.character(variable)
  for (i in 1:nrow(lool)) {

    variable.new[variable.new == lool[i, matching.column]] <- lool[i, replacement.column]
  }

  #print(Sys.time() - t)


  # Convert result to factor if input was factor, fix the order of levels in case it's requested

  if(save.levels.order  & is.factor(variable) ) {
    variable.new <- factor(variable.new, levels=
                             sapply(levels(variable), function(x) lool[lool[,matching.column]==x, replacement.column] ))
  } else if(!save.levels.order  & is.factor(variable) ) {
    variable.new <- as.factor(variable.new)
  } else {
    variable.new <- as.character(variable.new)
  }

  # Return
  variable.new
}



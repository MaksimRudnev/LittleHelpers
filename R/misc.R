#' Compare two correlation matrices
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param corr.X First correlation matrix, e.g. produced by `cor` function
#' @param corr.Y Secon correlation matrix, e.g. produced by `cor` function
#'
#' @export
compare_cor <- function(corr.X, corr.Y) {
  differences = merge(
    melt(corr.X),
    melt(corr.Y),
    by = c("Var1", "Var2"), suffixes = c(".X", ".Y")) %>%
    mutate(drop = apply(cbind(.$Var1, .$Var2), 1, function(x) any(duplicated(x)) )) %>%
    mutate(drop2 = apply(cbind(.$Var1, .$Var2), 1, function(x) which(colSums(t(cbind(.$Var2, .$Var1)) == x)==2) )) %>%
    mutate(drop3 = rownames(.) > drop2 ) %>%
    filter(!drop & !drop3) %>%
    mutate(diff = round(value.X - value.Y , 2)) %>%
    arrange(desc(abs(diff)))


  list(differences = differences,
       summary = c(
         average.raw.diff = mean(differences$diff),
         sd.raw.diff = sd(differences$diff),
         average.abs.diff = mean(abs(differences$diff)),
         sd.abs.diff =  sd(abs(differences$diff)),
         fisher.abs.diff = psych::fisherz2r(mean(psych::fisherz(abs(differences$diff)))),
         fisher.abs.dfiff.sd = psych::fisherz2r(sd(psych::fisherz(abs(differences$diff)))
         )))
}

#' Conditional formatting
#'
#' Adds formatting to a vector of parameters
#'
#' @param x Character vector
#' @param star Removes stars if FALSE.
#' @param bold.thresh Bold font if the absolute value is larger than this threshold.
#' @param neg.red Red font if the value is negative value.
#' @param small.thresh Replaces the value with `<|.01|` if the magnitude is smaller than the indicated number. Default is .01.
#' @param extra.style Add extra CSS style to the span tag.
#'
#' @details Can be used within `mutate()` and passed to `knitr::kable` with parameter `kable.options=list(escape=FALSE)`
#'
#'
#' @export
sig_to_bold <- function(x, star = F, bold.thresh = 1, neg.red = T,
                        small.thresh = .01, replace.small = NA, extra.style = "") {
  replace.small = ifelse(is.na(replace.small), paste0("&lt;|", sub("0\\.", "\\.", small.thresh), "|"), replace.small)
  enclose.bold = function(y) paste0("<b>", y, "</b>")
  enclose.red = function(y) paste0('<span style="color: red;">', y, "</span>")
  enclose.style = function(y) paste0('<span style="', extra.style, '">', y, '</span>')

  ifelse(is.na(x), NA, {
    x.out = sub("0\\.", "\\.", x)
    x.num = as.numeric(gsub("\\*+", "", x))
    if(!star) x.out = gsub("\\*+", "", x.out)
    if(neg.red) x.out = ifelse(x.num<0, enclose.red(x.out), x.out)
    if(bold.thresh < 1) {
      x.out = ifelse(abs(x.num)>bold.thresh, enclose.bold(x.out), x.out) }
    if(small.thresh != 0) {
      x.out = ifelse(abs(x.num)<small.thresh, replace.small, x.out) }
    if(extra.style!="") x.out = enclose.style(x.out)
    return(x.out)
  })}



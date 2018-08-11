#' Get more comprehensible output from lavTestScore
#'
#' Simply applies \code{\link{lavTestScore}} and attached group labels and parameter names to the output.
#'
#' @param lavaan.fit Model fitted with lavaan
#' @param ... Arguments passed to lavTestScore
#'
#' @export

lavTestScore_clean <- function(lavaan.fit,  ...) {
  require("lavaan")
  lvts <- lavTestScore(lavaan.fit, ...)

  for(lvts.part in names(lvts)[names(lvts) %in% c("uni", "cumulative")]) {

    partab.a<- partable(lavaan.fit)[,c(c("lhs", "op",  "rhs", "group",  "plabel"))]  %>%
      dplyr::filter(., plabel!="")

    names(partab.a)[1:3] <- c("one", "two", "three")

    out<- merge(as.data.frame(lvts[[lvts.part]]),
                partab.a,
                by.x=c("lhs"), by.y=c("plabel"),
                all.x=T)
    out2<- merge(out,
                 partab.a,
                 by.x=c("rhs"), by.y=c("plabel"),
                 all.x=T, suffixes = c(".lhs", ".rhs"))

    out2$group.lhs <- factor(out2$group.lhs, levels=1:length(lavInspect(lavaan.fit, "group.label")), labels=lavInspect(lavaan.fit, "group.label"))
    out2$group.rhs <- factor(out2$group.rhs, levels=1:length(lavInspect(lavaan.fit, "group.label")), labels=lavInspect(lavaan.fit, "group.label"))

    out3 <- data.frame(Term = paste(out2$one.lhs, out2$two.lhs, out2$three.lhs, sep=""),
                       Group1 = out2$group.lhs,
                       Group2 = out2$group.rhs,
                       Chi.square=round(out2$X2, 3), df=out2$df, p.value=round(out2$p.value,3),
                       "."=format(as.character(sapply(out2$p.value, function(x) ifelse(x>0.05, "", ifelse(x>0.01, "*", ifelse(x>0.001, "**", "***"))))), justify = "left")
    )

    lvts[[lvts.part]]<-out3
    if(lvts.part=="uni") attr(lvts[[lvts.part]], "header") <- "Chi-square test of releasing single constraints, equivalent to modification indices"
    if(lvts.part=="cumulative") attr(lvts[[lvts.part]], "header") <- "Chi-square test of releasing multiple constraints at the same time"
    class(lvts[[lvts.part]]) <- c("lavaan.data.frame","data.frame")

  }


  if(any(names(lvts) == c("epc"))) {
    lvts[["epc"]]$group <- factor(lvts[["epc"]]$group,
                                  levels=1:length(lavInspect(lavaan.fit, "group.label")),
                                  labels=lavInspect(lavaan.fit, "group.label"))
  }


  return(lvts)


}

#' Quick diagnostics for multiple-group lavaan models
#' @param lavaan.model Fitted lavaan multiple group model.
#' @param output A character list of what diagnostics should be computed. See Details."
#'
#' @details Here are possible values for output:
#'
#' \describe{
#'   \item{_overall_}{Prints chi-sq, CFI, RMSEA, and SRMR }
#'   \item{_neg.var_}{Checks if there are negative variances of latent variables, if yes, identifies the groups and prints them.}
#'   \item{_mi_}{Aggregates modification indices across groups, finding the most impactful. Also prints the top part of sorted table of modification indices.}
#'   \item{_constraints_}{Applies and prints \code{\link{lavTestScore_clean}}.}
#' }
#'
#' @md
#'
#' @export

mgcfa_diagnose <- function(lavaan.model, output=c("overall", "neg.var", "mi", "constraints")) {
  require(magrittr)
  if("overall" %in% output) {
    # Print general info
    cat(paste("\n\n  Model fitted to ", lavInspect(lavaan.model, "ngroups"), "groups, and ",
              sum(lavInspect(lavaan.model, "nobs")), "observations.\n"))

    # Print overall fit measures
    cat("\n\n  Overall fit measures: \n")
    a<-fitmeasures(lavaan.model)[c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr")]
    print(data.frame(Index=round(a[c(1,4,5,6)],3),
                     df=c(round(a[2]),  rep("", 3)),
                     p.value=c(round(a[3],4),  rep("", 3))
    ), na.print=NULL, quote=F, print.gap=4)
  }
  # Print negative LV variances if there are any
  if("neg.var" %in% output) {

    a<-lavInspect(lavaan.model,"cov.lv") %>% sapply(diag) %>% t %>% round(5)
    if(any(a<0)) {
      cat("\n\n  Negative LV variances found: \n")
      if(sum(rowSums(a<0))==1) {
        data.frame(a[rowSums(a<0)>0,]) %>% t %>%
          `row.names<-`(row.names(a)[rowSums(a<0)>0]) %>%
          format(digits=4) %>% print(quote=F, print.gap = 4)
      } else {
        a[rowSums(a<0)>0,] %>%
          format(digits=4) %>% print(quote=F, print.gap = 4, row.names=T)
      }
    } else {
      cat("\n\n  All latent variables' variances are positive. \n")
    }
    rm(a)

    b<-lavInspect(lavaan.model,"cov.ov") %>% sapply(diag) %>% t %>% round(5)
    if(any(b<0)) {
      cat("\n\n  Negative LV variances found: \n")
      if(sum(rowSums(b<0))==1) {
        data.frame(b[rowSums(b<0)>0,]) %>% t %>%
          `row.names<-`(row.names(b)[rowSums(b<0)>0]) %>%
          format(digits=4) %>% print(quote=F, print.gap = 4)
      } else {
        b[rowSums(b<0)>0,] %>%
          format(digits=4) %>% print(quote=F, print.gap = 4, row.names=T)
      }
    } else {
      cat("\n\n  All residuals' variances are positive. \n")
    }
    rm(b)

  }
  # Find most common misspecisfications
  if("mi" %in% output) {
    a<-try(modindices(lavaan.model, sort.=T))

    if(any(class(a)=="try-error")) {
      cat("\n\n  *modification indices cannot be computed.")
    } else {
      cat("\n\n  Aggregate modification indices \n")
      aggregate(a$mi, list(paste(a$lhs,a$op, a$rhs)), sum) %>%
        `names<-`(c("Term", "Sum of modif.indices across groups")) %>%
        print(right = F, row.names=F)
      # Head of modification indices list
      cat("Head of modification indices list \n")
      print(as.data.frame(a)[1:10,], row.names=F)
      cat("* Use 'modindices()' to see the full list.")
    }
    rm(a)
  }
  # Find problems with constraints
  if("constraints" %in% output) {
    cat("\n\n What if some constraints are relaxed?  \n")
    try(lavTestScore_clean(lavaan.model))
  }
}


#' Classic measurement invariance test
#'
#' @export


mi_test<- function(lavaan.model, ...) {

  if(any(class(lavaan.model)=="lavaan")) {
    conf <- update(lavaan.model, group.equal="none")
    metric <- update(lavaan.model, group.equal=c("loadings"))
    scalar <- update(lavaan.model, group.equal=c("loadings", "intercepts"))
    means <- update(lavaan.model, group.equal=c("loadings", "intercepts", "means"))


  } else if(any(class(lavaan.model)=="formula") | any(class(lavaan.model)=="character")) {

    conf <- cfa(lavaan.model, group.equal="none", ...)
    metric <- cfa(lavaan.model, group.equal=c("loadings"), ...)
    scalar <- cfa(lavaan.model, group.equal=c("loadings", "intercepts"), ...)
    means <- cfa(lavaan.model, group.equal=c("loadings", "intercepts", "means"), ...)

  } else {
    error("Argument should be a fitted lavaan model or a lavaan formula.")

  }

  tt<-data.frame(conf=fitmeasures(conf)[c("chisq", "df", "cfi", "rmsea", "srmr")],
                 metric=fitmeasures(metric)[c("chisq", "df", "cfi", "rmsea", "srmr")],
                 scalar=fitmeasures(scalar)[c("chisq", "df", "cfi", "rmsea", "srmr")],
                 means=fitmeasures(means)[c("chisq", "df", "cfi", "rmsea", "srmr")]) %>% t %>% as.data.frame
  tt$delta.CFI <- c(NA, tt$cfi[1:(length(tt$cfi)-1)] - tt$cfi[2:length(tt$cfi)])
  tt$delta.RMSEA <- c(NA, abs(tt$rmsea[1:(length(tt$rmsea)-1)] - tt$rmsea[2:length(tt$rmsea)]))
  tt$delta.SRMR <- c(NA, abs(tt$srmr[1:(length(tt$srmr)-1)] - tt$srmr[2:length(tt$srmr)]))

  print(with(tt, data.frame(chisq=round(chisq, 2),
                         df=round(df),
                         CFI = round(cfi, 3),
                         RMSEA = round(rmsea, 3),
                         SRMR = round(srmr, 3),

                         delta.CFI = format(delta.CFI, digits=1, nsmall=3, scientific =FALSE),
                         delta.RMSEA = format(delta.RMSEA, digits=1, nsmall=3, scientific =FALSE),
                         delta.SRMR = format(delta.SRMR, digits=1, nsmall=3, scientific =FALSE),
                         row.names = rownames(mit.3))))
  invisible(tt)

}



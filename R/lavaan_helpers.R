#' Get more comprehensible output from lavTestScore
#'
#' Simply applies \code{\link{lavTestScore}} and attaches group labels and parameter names to the output.
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


# Classic measurement invariance test
# mi_test<- function(lavaan.model, ...) {
#
#   if(any(class(lavaan.model)=="lavaan")) {
#     conf <- update(lavaan.model, group.equal="none")
#     metric <- update(lavaan.model, group.equal=c("loadings"))
#     scalar <- update(lavaan.model, group.equal=c("loadings", "intercepts"))
#     means <- update(lavaan.model, group.equal=c("loadings", "intercepts", "means"))
#
#
#   } else if(any(class(lavaan.model)=="formula") | any(class(lavaan.model)=="character")) {
#
#     conf <- cfa(lavaan.model, group.equal="none", ...)
#     metric <- cfa(lavaan.model, group.equal=c("loadings"), ...)
#     scalar <- cfa(lavaan.model, group.equal=c("loadings", "intercepts"), ...)
#     means <- cfa(lavaan.model, group.equal=c("loadings", "intercepts", "means"), ...)
#
#   } else {
#     error("Argument should be a fitted lavaan model or a lavaan formula.")
#
#   }
#
#   tt<-data.frame(conf=fitmeasures(conf)[c("chisq", "df", "cfi", "rmsea", "srmr")],
#                  metric=fitmeasures(metric)[c("chisq", "df", "cfi", "rmsea", "srmr")],
#                  scalar=fitmeasures(scalar)[c("chisq", "df", "cfi", "rmsea", "srmr")],
#                  means=fitmeasures(means)[c("chisq", "df", "cfi", "rmsea", "srmr")]) %>% t %>% as.data.frame
#   tt$delta.CFI <- c(NA, tt$cfi[1:(length(tt$cfi)-1)] - tt$cfi[2:length(tt$cfi)])
#   tt$delta.RMSEA <- c(NA, abs(tt$rmsea[1:(length(tt$rmsea)-1)] - tt$rmsea[2:length(tt$rmsea)]))
#   tt$delta.SRMR <- c(NA, abs(tt$srmr[1:(length(tt$srmr)-1)] - tt$srmr[2:length(tt$srmr)]))
#
#   print(with(tt, data.frame(chisq=round(chisq, 2),
#                          df=round(df),
#                          CFI = round(cfi, 3),
#                          RMSEA = round(rmsea, 3),
#                          SRMR = round(srmr, 3),
#
#                          delta.CFI = format(delta.CFI, digits=1, nsmall=3, scientific =FALSE),
#                          delta.RMSEA = format(delta.RMSEA, digits=1, nsmall=3, scientific =FALSE),
#                          delta.SRMR = format(delta.SRMR, digits=1, nsmall=3, scientific =FALSE),
#                          row.names = rownames(mit.3))))
#   invisible(tt)
#
# }

#' Run three models of invariance and compare them
#' @description This mimicking the deprecated semTools::measurementInvariance()
#' @param lavaan.model Either a fitted lavaan model object, or just a character formula model in lavaan standards
#' @param ... any \link[lavaan]{cfa} arguments except for 'group.equal'
#' @aliases mi_test
#' @seealso lavaan::cfa
#'
#' @export
measurementInvariance <- function(lavaan.model, ...) {

  if(any(class(lavaan.model)=="formula") | any(class(lavaan.model)=="character")) {

  r.conf<-lavaan::cfa(lavaan.model, ...)
  r.metric<-lavaan::cfa(lavaan.model, ..., group.equal = "loadings")
  r.scalar<-lavaan::cfa(lavaan.model, ..., group.equal = c("loadings", "intercepts"))
  r.means<-lavaan::cfa(lavaan.model, ..., group.equal = c("loadings", "intercepts", "means"))

  } else if(any(class(lavaan.model)=="lavaan") ) {

    r.conf<-update(lavaan.model, ...)
    r.metric<-update(lavaan.model, ..., group.equal = "loadings")
    r.scalar<-update(lavaan.model, ..., group.equal = c("loadings", "intercepts"))
    r.means<-update(lavaan.model, ..., group.equal = c("loadings", "intercepts", "means"))
  } else {
    error("Argument should be a fitted lavaan model or a lavaan formula.")

  }


  #print(r.conf@call[-3])

  out <- lavaan::lavTestLRT(r.conf, r.metric, r.scalar, r.means, model.names = c("Configural", "Metric", "Scalar", "Means"))
  #out <- out[,names(out)[c(4, 1, 5, 6)]]
  out2<-t(sapply(list(r.conf, r.metric, r.scalar, r.means),   fitmeasures)[c("cfi", "tli", "rmsea", "srmr"),])
  out2.2 <- apply(out2, 2, function(x) (c(NA, x[2]-x[1], x[3]-x[2], x[4]-x[3] )))
  colnames(out2.2)<- paste("∆", toupper(colnames(out2.2)), sep="")
  out2.3 <- t(Reduce("rbind", lapply(1:ncol(out2), function(x) rbind(out2[,x], out2.2[,x]))))
  colnames(out2.3)<-toupper(as.vector(sapply(1:length(colnames(out2)), function(i) c(colnames(out2)[i], colnames(out2.2)[i]) )))
  rownames(out2.3)<-c("Configural", "Metric", "Scalar", "Means")

  print(out)
  cat("\n")
  print(round(out2.3, 3), digits=3, row.names = TRUE, na.print = "" )

  invisible(list(out, out2.3))
}


#' Create a path diagram using lavaan syntax
#'
#' @description Converts lavaan syntax or object to graphviz code. Requires 'DiagrammeR' package.
#' @param m character, following lavaan syntax model conventions (see examples), or fitted lavaan object.
#' @param file character, file name to save svg code, usually with 'svg' extension.
#' @param rmarkdown Logical. If the function used in the context of Rmarkdown.
#' @param ... arguments passed to DiagrammeR::grViz function.
#'
#' @examples lav_to_graph("F =~ a1 + a2 + a3 + a4")
#' lav_to_graph("F =~ a1 + a2 + a3 + a4; a1 ~~ a2; d ~ F ", engine = "neato")
#' @return The function invisibly returns the dot code which can be edited to make more customized diagrams. You can use package \pkg{DiagrammeR} or any other graphviz engine, e.g. \url{http://graphviz.it}. It will most likely to be useful with large and complex models.
#'
#' @export
lav_to_graph <- function(m, layout = "dot", adds=NULL, file=NA, rmarkdown=FALSE, ...) {
  require("DiagrammeR")

  if(class(m)=="lavaan") {
    pt <- lavaan::parameterTable(m)
    pt <- pt[pt$rhs!="",]
    message("Currently, intercepts are not supported.")
    st<-pt$est/pt$se>1.96
    st[st==T]<-"^^"
    st[st=="FALSE"]<-""
    m <- paste0(pt$lhs, " ", pt$op, " ", sprintf("%1.2f",pt$est), st, "*", pt$rhs,
                collapse=";\n")
  }

    # m <- "F =~ a1 + a2 + a3 ;
    #                      F2 =~ b1 + b3 + NA*hh;
    #       b1 + b3 ~~ 0*a3 + NA*a2 + G;"
  m <- gsub(" +", " ", m)
  m <- gsub("\\+ *\n", " + ", m)
  m <- gsub(";", "\n", m)
  m <- gsub("\n\n", "\n", m)
  m <- gsub("\n ", "\n", m)
  m <- gsub(" \n ", "\n", m)
  m <- gsub("  ", " ", m)

  m<-strsplit(m, "\n")[[1]]
  m<- gsub("#.*", "", m)
  m <- m[!m==""]

  all.vars <- unname(unlist(sapply(m, strsplit, "=~|~~|~|\\+")))
  all.vars <- gsub(" ", "", all.vars)
  all.vars <- gsub("^.*\\*", "", all.vars)
  all.vars <- all.vars[!duplicated(all.vars)]
  all.vars <- all.vars[all.vars!=1]

  all.factors <- sapply(m[grepl("^.*=~", m)], function(x) strsplit(x, "=~")[[1]][1] )
  all.factors <- unname(gsub(" ", "", all.factors))


  all.indicators <- sapply(m[grepl("^.*=~", m)], function(x) strsplit(x, "=~")[[1]][2] )
  m<-m[!grepl("^.*=~", m)]
  m <- append(unlist(lapply(unique(all.factors), function(x) paste(x, "=~", paste(all.indicators[all.factors==x], collapse=" + ")    ))), m)

  if(length(all.factors)!=0) {
   all.indicators <- unlist(strsplit(all.indicators, "\\+"))
   all.indicators<- gsub(" ", "", all.indicators)
   all.indicators <- unname(gsub("^.*\\*", "", all.indicators))
  }
  observed.vars <- all.vars[!all.vars %in% all.factors]

  regr <- m[grepl("~", m) & !grepl("~~|=~", m)]
  dep.regr.vars <- gsub("~.*$", "", regr)
  dep.regr.vars <- gsub(" ", "", dep.regr.vars)

  endogenous.vars <- c(all.indicators, dep.regr.vars)
  exogenous.vars <- all.vars[!all.vars %in% endogenous.vars]





  all.correlated <- sapply(m[grepl("^.*~~", m)], function(x) strsplit(x, "~~")[[1]] )
  if(length(all.correlated)>0) {
  all.correlated <- apply(all.correlated, 2, function(x) {
    a = strsplit(x, "\\+")
    a = lapply(a, function(u) gsub(" ", "",u))
    a = expand.grid(a,  stringsAsFactors = F)
    a$constraint<- sapply(a[,2], function(u) ifelse(grepl("\\*", u), sub("\\*.*", "", u), NA))
    a[,2]<-gsub("^.*\\*", "",a[,2])
    a[,3][a[,3]=="NA"]<-NA
    a
    })

   all.correlated <- Reduce("rbind", all.correlated)
   all.exog.cors <- expand.grid(exogenous.vars, exogenous.vars,  stringsAsFactors = F)


   # from all.exog.cors  exclude  all.correlated

   excl.corr <- apply(all.exog.cors, 1, function(x) {

     (x[1] %in% all.correlated$Var1) & (x[2] %in% all.correlated$Var2) |
     (x[2] %in% all.correlated$Var1) & (x[1] %in% all.correlated$Var2)
   })

   all.exog.cors <- all.exog.cors[!excl.corr,]

  } else {
    all.exog.cors <- expand.grid(exogenous.vars, exogenous.vars,  stringsAsFactors = F)
  }

  all.exog.cors <- all.exog.cors[!all.exog.cors[,1]==all.exog.cors[,2],]

  if(length(unlist(all.exog.cors))>0) {

  excl.dupl.corr <- sapply( 1:nrow(all.exog.cors), function(x) which(
    (all.exog.cors[x,2] == all.exog.cors[x:nrow(all.exog.cors),1]) &
      (all.exog.cors[x,1] == all.exog.cors[x:nrow(all.exog.cors),2]))+x-1)
  all.exog.cors <- all.exog.cors[ -unlist(excl.dupl.corr),]
  }

  # all.factors.cors.excl <- apply(all.factors.cors,1, function(x) any(apply(all.correlated, 1, function(u) {
  #   sum(u[1:2] == x)==2 & u[3]==0 | sum(u[2:1] == x)==2 & u[3]==0
  # })))
  #
  # all.factors.cors <-all.factors.cors[(!all.factors.cors.excl) &
  #                                       all.factors.cors[,1]!=all.factors.cors[,2],]
  #
  #


  lines <-
    sapply(m, function(i) {

      if(grepl("=~", i)) {
        m0 <- strsplit(i, "=~")[[1]]
        factr <- gsub(" ", "", m0[1])
        factr  <- `names<-`(gsub("\\.|-", "_", factr), factr) #values are dotless, names are original varnames

        indicators <- strsplit(m0[2], "\\+")[[1]]
        indicators <- gsub(" ", "", indicators)

        #params <- rep("", length(indicators))

        params = gsub("\\*.*$", "", indicators[grep("\\*", indicators)])
        params = gsub("NA", "", params)
        params[params!=""] <- paste0("[label=",params,"]")[params!=""]

        indicators <- gsub("^.*\\*", "", indicators)
        indicators <- `names<-`(gsub("\\.|-", "_", indicators), indicators) #values are dotless, names are original varnames

        c(paste0("\n\nsubgraph cluster_", factr, '  {\n  color = white;\n'),
          paste0("\t", factr, ' [shape = ellipse label = ', paste0('"', names(factr), '"'), '];\n'),
          if(any(!names(indicators) %in% all.factors)) paste0("\t",
                                                      indicators[!names(indicators) %in% all.factors],
                                                      '[shape = rect label = ', paste0('"', names(indicators), '"'), '];\n'),

          paste(paste0("\tResid_", indicators),
                '[shape = circle style = filled color=lightgrey',
                'fontsize = 10 width= 0.2 label = "&epsilon;"];\n',
                collapse = ""),
          paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "min", "max"),
                paste(paste0("Resid_",indicators), collapse = " "), "};\n"),
          paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "max", "min"),
                factr, "};\n"),

          paste0("\t",  factr, " -> ",  indicators, params, ";\n"),

          paste0("\t", paste0("Resid_",indicators), " -> ", indicators, "[color=grey];\n"),
          paste("}\n"),
          paste(" // end of ", names(factr), "measurement model.",
                names(factr), " is ", ifelse(names(factr) %in% exogenous.vars, "exogenous.\n\n", "endogenous.\n\n"))
        )

      } else if(grepl("(~~)", i)) {

        m0  <- strsplit(i, "~~")[[1]]
        lhs <- strsplit(m0[1], "\\+")[[1]]
        lhs <- gsub(" ", "", lhs)
        lhs <- gsub("^.*\\*", "", lhs)
        lhs <- `names<-`(gsub("\\.|-", "_", lhs), lhs) #values are dotless, names are original varnames


        rhs <- strsplit(m0[2], "\\+")[[1]]
        rhs <- gsub(" ", "", rhs)
        pars <- rep(NA, length(rhs))
        pars[grep("\\*", rhs)] <- gsub("\\*.*$", "", rhs[grep("\\*", rhs)])

        # paste0("[label=",
        #        gsub("\\*.*$", "", rhs[grep("\\*", rhs)]),
        #        "]")

        rhs <-gsub("^.*\\*", "", rhs)
        rhs <- `names<-`(gsub("\\.|-", "_", rhs), rhs) #values are dotless, names are original varnames

        # turn to residuals if variables are indicators
        lhs[names(lhs) %in% all.indicators] <- paste0("Resid_", lhs[names(lhs) %in% all.indicators])
        rhs[names(rhs) %in% all.indicators] <- paste0("Resid_", rhs[names(rhs) %in% all.indicators])


       covs <- data.frame(lhs=unname(lhs), rhs=unname(rhs), pars, stringsAsFactors = F)

       sapply(1:nrow(covs), function(co) {
         co <- covs[co,]
         if(co$lhs == co$rhs) {
           paste(
             paste0(co$rhs, '[xlabel="&sigma;&sup2;&#61;', as.character(co$pars), '"];\n'),
             ifelse(co$pars==0, paste0(co$rhs, '[style=dashed];\n'),"")
           )

         } else  {


             if(!is.na(co$pars)) {
               if(co$pars!=0)
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false label=', co$pars, ' fontsize = 10 ];\n')
             } else {
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false];\n')

               }
         }
       })

      } else if(grepl("~", i) & !grepl("~~|=~", i)) {




        m0 <- strsplit(i, "~")[[1]]
        dep <- strsplit(m0[1], "\\+")[[1]]
        dep <- gsub(" ", "", dep)
        dep<-dep[!dep==""]
        dep  <- `names<-`(gsub("\\.|-", "_", dep), dep) #values are dotless, names are original varnames

        indep <- strsplit(m0[2], "\\+")[[1]]
        indep <- gsub(" ", "", indep)
        indep<-indep[!indep==""]
        cof = rep("", length(indep))
        cof[grep("\\*", indep)] <- gsub("\\*.*$", "", indep[grep("\\*", indep)])
        indep <- gsub("^.*\\*", "", indep)
        indep  <- `names<-`(gsub("\\.|-", "_", indep), indep) #values are dotless, names are original varnames

        intercepts <- indep[indep == 1]
        indep      <- indep[indep != 1]
        reg.otp<-"// Regressions and intercepts \n"
        if(length(indep)>0) {
         reg.otp <- append(reg.otp, c(
              paste(dep, "[label=", paste0('"', names(dep), '"'), "];\n"),
              paste(indep, "[label=", paste0('"', names(indep), '"'), "];\n"),
              paste0(indep, " -> ",  dep, ' [style=bold ', paste0('label="',cof)[cof!=""],'"];\n'),
              paste(paste0("Resid_",dep), ' [shape = circle style = filled color=lightgrey',
                    'fontsize=10 width=0.2, label = "&epsilon;"];\n'),
              paste0(paste0("Resid_",dep), " -> ", dep, " [color=grey];\n")
            ))
        }

        if(length(intercepts)>0) {
          reg.otp <- append(reg.otp,
           c(paste(dep, "[label=", paste0('"', names(dep), '"'), "];\n"),

            paste0(paste0('intercept_', dep),
                  ' [label="&tau;', ifelse(length(cof)!=0, paste0('&#61;', cof), ''),
                  '" shape=triangle', ifelse(cof==0, ' style=dashed', ' style=filled'),
                  '  color=lightgrey];\n'),
            paste0(paste0('intercept_', dep), ' -> ',  dep, ' [color=lightgrey];\n')
          ))
        }

        reg.otp


      } else  { # if(i!="")
        warning("Unknown operator found in line: ", i)
      }
    })

  # Add covariances between exogenous vars
  lines <- append(lines, c("\n\n// Add covariances of all exog variables\n",
                  if(nrow(all.exog.cors)>1) {
                    all.exog.cors  <- sapply(all.exog.cors, function(u) gsub("\\.|-", "_", u))
                  apply(all.exog.cors,
                        1,
                        function(x) paste(x[1], "->", x[2],
                                          '[dir = "both" splines=curved constraint=false color=grey];\n')
                        )
                  } else if(length(unlist(all.exog.cors))>0) {
                    paste(all.exog.cors[1], "->", all.exog.cors[2], '[dir = "both" splines=curved constraint=false color=grey];\n')


                  } else {

                    paste("")
                  }
                  ))

  lines <- unlist(lines)
  excl.lines <- duplicated(lines)
  excl.lines [ lines == "}\n"] <- FALSE
  lines <- lines[!excl.lines]

  lines <- paste("digraph lav_to_graph {",
                  " node [shape=rect];",
                 " rankdir=LR;",
                 paste0(" layout=", layout, ";"),
                 " forcelabels=true;",
                 paste0(lines, collapse = ""),
                 ifelse(is.null(adds), "", paste("// code added manually \n", paste(adds, collapse="\n" ))),
                 "}",
                 sep="\n")

  lines <- gsub("^^", "*", lines, fixed = T)

if(!rmarkdown) {
  trash<- capture.output(DiagrammeR::grViz(lines, ...))
  #cat(lines)
  if(!is.na(file)) {
    require("DiagrammeRsvg")
    cat(DiagrammeRsvg::export_svg(DiagrammeR::grViz(lines, ...)), file=file)
  }
  invisible(lines)
} else {
  DiagrammeR::grViz(lines, ...)
  }

}
# a=Sys.time()
# lav_to_graph(mdl, adds = "rankdir='LR'")
# Sys.time()-a
# # #
#
# lav_to_graph("a ~ b; b ~ c; c ~ d; a ~ d0 ", adds = "rankdir='BT'")
#
#
# ess7 <- haven::read_sav("~/Dropbox/STAT/European Social Survey/data/ESS7/ESS7e02_1.sav")
# Austria <- ess7[ess7$cntry == "AT",]
#
# library(lavaan)
# cfa1 <- cfa( 'F1 =~ ipadvnt + impfun + impdiff + ipgdtim;
#              F2 =~  ipcrtiv+ impfree;
#              impfun ~~ ipgdtim;
#              ', data=Austria)
#
# lav_to_graph(cfa1)
#
# mdl <- "institutionalized =~ NA*contact+ NA*workorg;
# non.institutionalized =~ NA*petition + NA*demonstr + NA*boycott;
# institutionalized ~~ 1*institutionalized;
# non.institutionalized  ~~ 1*non.institutionalized;
# associationalism =~ NA*voluntary + NA*participate; associationalism~~1*associationalism;
# closeness =~ 1*close.party; close.party ~~ 0*close.party;
# social.trust =~ NA*trust + NA*fairness; social.trust ~~ 1*social.trust;
# information =~ NA*tv + NA*radio; information ~~ 1*information;
# pol.interest =~ 1*interest; interest ~~ 0*interest;
# sociability =~ 1*soc.activity; soc.activity ~~ 0*soc.activity;
# institutionalized ~ associationalism + closeness + social.trust +
#     information + pol.interest + sociability + non.institutionalized;
#  non.institutionalized ~ associationalism + closeness + social.trust +
#     information + pol.interest + sociability + institutionalized
#     institutionalized ~~ 0*institutionalized
#     institutionalized ~ 0*1
# "
# lav_to_graph(mdl, file = "~/Downloads/hopahopa.svg")
#
# lav_to_graph('F =~ 1*a1 + 2*a2 + NA*a3
#     F2 =~ a4 + a5 + a6
#     G =~ NA*a1 + a2 + a3 + a4 + a5 + a6;
#     G ~~ 0*G;
#     a1~~ 0.5*a2;
#     a1 ~~ 5*a1;
#     G ~ 1*F2;', adds="splines=false;")


#' Combine fit measures form several models and compare
#' @details  Extracts select fit measures from lavaan objects and lists it in the tables.
#' @param ... lavaan fitted objects
#'
#' @examples lav_compare(fit1, fit2, fit3)
#'
#'
#' @export
lav_compare <- function(...) {


  out <- lavaan::lavTestLRT(..., model.names = as.character(substitute(...()) ) )

  out2<-t(sapply(list(...),   fitmeasures)[c("cfi", "tli", "rmsea", "srmr", "bic"),])
  rownames(out2)<-as.character(substitute(...()) )


  print(out)
  cat("\n")
  print(round(out2, 3), digits=3, row.names = TRUE, na.print = "" )

  invisible(list(LRT=out, Other.fit=out2))
}



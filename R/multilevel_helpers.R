#' Function producing a tidy table for many multilevel regressions (lmer fitted objects)
#' @description Stacks coefficients and SEs, extracts various lmer-specific model fit statistics and confidence intervals for random effects (refits models if necessary), outputs a nicely formatted table using stargazer and shows it directly in Rstudio Viewer.
#' @param models List of the fitted lmer objects
#' @param fit.stats What fit statistics to compute/extract and show? Possible options are "ICC", "random", "R2s", "fit", "LRT", "REML", "VIF". See "Details".
#' @param mod.names Vector of the same length as models list, giving names to each model.
#' @param show.viewer Logical. Whether the resulting table should be shown in the RStudio viewer. If FALSE then the file "good_table_output.html" is saved to working directory.
#' @param ... Arguments passed to stargazer.
#'
#'
#'@examples \donttest{
#' m1<- lmer(HE ~ age + gender + (1|country), ess.data.d, weights=dweight)
#' m2<- lmer(HE ~ age + gender + (1+age + gender|country), ess.data.d, weights=dweight)
#' good_table(list(m1, m2))
#'
#' # Another option
#' f1 <- as.formula(HE ~ age + gender + (1|country))
#' f2 <- as.formula(HE ~ age + gender + (1|country))
#' good_table(lapply(c(f1, f2), function(x) lmer(x, ess.data.d, weights=dweight)))
#'}
#'
#'
#' @details  The list of possible fit.stats options:
#'  \describe{
#'   \item{fit}{Shows deviance (-2*logLikelihood), AIC, BIC, number of parameters, number of groups, number of observations, if the model converged}
#'   \item{ICC}{Computes intra-class correlation by fitting an empty model and computing a ratio of first-level and intercept variances.   }
#'   \item{R2}{Computes R-square by fitting an empoty model and computing a ratio of residuals in an empty model and in the current model. }
#'   \item{LRT}{Computes Likelihood ratio test; all the models should be fitted to the same sample and be in order of nestedness, otherwise the test fails. }
#'   \item{random}{Adds variances of all the random effects.}
#'   \item{random.p}{Adds variances of all the random effects and implements bootstrapping (`lme4::confint.merMod`) in order to get confidence intervals and this p.values for variances of random effects. }
#'   \item{REML}{Shows if REML was used to fit the model.}
#' }
#'
#'
#'@note Issues to implement:
#' * Might be veeery slow (something to work on)
#'
#' @md
#' @aliases lmer_table
#' @export
good_table <- function(models,
                       fit.stats=c("ICC", "random", "random.p", "R2", "fit", "LRT", "REML", "VIF"),
                       mod.names="",
                       show.viewer=TRUE,
                       silent = TRUE,
                       #boot.ci = FALSE,
                        ...) {
  require(scales)
  require(stargazer)
  total.time<-Sys.time()




  if(mod.names=="") mod.names <- rep("", length(models))
  if( sum(c("random","random.p") %in% fit.stats)==2) {
    message("You have to use EITHER random OR `random.p` for `fit.stats`. For now I removed `random.p` and use `random` only.")
    fit.stats<-fit.stats[!fit.stats=="random.p"]
    }


  myICC <- function(lmer_model) {
    my_variances<- as.data.frame(lme4::VarCorr(lmer_model))$vcov
    icc<-my_variances[1]/sum(my_variances[1],my_variances[length(my_variances)])
    paste(round(icc,2)*100, "%", sep="")
  }


  if("LRT" %in% fit.stats | "fit" %in% fit.stats ) {

   if( any(!lapply(models, nobs)==nobs(models[[1]])) ) {
     message("At least one model wasn't fit to the same sample, so LRT computation is stopped.")
   } else {


        if(any(sapply(models, isREML))) {
          if(!silent)   message("Refitting model with ML estimator to extract correct deviance and compute information criteria. \n Might take a long time.")
          if(!silent)  pb<-txtProgressBar(0, length(models), label="Going progress bar...", style = 3)
            modelsML<- lapply(1:length(models), function(m) {
                refitML(models[[m]])
              if(!silent)   utils::setTxtProgressBar(pb, m)
                })

        } else {
          modelsML <- models
        }
   }
    }



  if("random" %in% fit.stats | "random.p" %in% fit.stats ) {

  random.variances<-lapply(models, function(x) {
    variances<-c(attr(lme4::VarCorr(x)[[1]], "stddev")^2, Residual=attr(lme4::VarCorr(x), "sc")^2 )
    data.frame(variances, names=paste("Variance of", names(variances)), stringsAsFactors = F)
                      } )
  random.variances<-suppressWarnings(Reduce(function(x,y) merge(x,y, by="names", all =T, suffixes=letters), random.variances))

 if(!silent) {
   verb("random.variances")
   print(random.variances)
 }

    if(!"random.p" %in% fit.stats) {
       random <-lapply(1:nrow(random.variances), function(x) unname(c(random.variances[x,1], round(unlist(random.variances[x,-1]), 3)  )))
    }
  }


  if("random.p" %in% fit.stats) {

   message("Computing bootstrapped p-values. Usually it takes a long time. Be patient, or interrupt and use fit.stats='random' instead of 'random.p'. ")
    boot.models <- lapply(models, function(x) {
      confint.merMod(x, nsim=100,
                     parm="theta_",
                     boot.type = "basic",
                     method = "boot",
                     parallel="multicore",
                     ncpus=4, oldNames=F)
      })

   p<- lapply(boot.models, function(a) {
                b<- a[grepl("sd_|sigma", dimnames(a)[[1]] ),]
              dimnames(b)[[1]]<-gsub("sd_|\\|cntry", "",   dimnames(b)[[1]])
              dimnames(b)[[1]]<-gsub("sigma", "Residual",   dimnames(b)[[1]])
              c<- b[,1]*b[,2]>0
              c[c]<-"#"
              c[c=="FALSE"]<-""
              data.frame(`sig.`=c, names=names(c))
              })

   p1<- Reduce(function(x,y) merge(x,y, by="names", all =T), p)
   rownames(p1) <- p1$names
   p1<- p1[gsub("Variance of ", "", random.variances$names),]

   random.p <-lapply(1:nrow(random.variances), function(x) unname(c(random.variances[x,1],
                                                                  paste(round(unlist(random.variances[x,-1]), 3), unlist(p1[x,-1]), sep="")
                                                                  )))



   }



  if("R2" %in% fit.stats ) {
    message("Computing zero models to get absolute R-squares (reduction in residual variances).")

    M0 <- lapply(models, function(x)  update(x,  paste(".~ 1+ (1|",   names(getME(x, "flist")), ")")   ))
    random.variances.M0<-sapply(1:length(M0), function(x) {
      c(
        R2.intercept=
          (attr(lme4::VarCorr(M0[[x]])[[1]], "stddev")[[1]]^2 -
             attr(lme4::VarCorr(models[[x]])[[1]], "stddev")[[1]]^2)/
          (attr(lme4::VarCorr(M0[[x]])[[1]], "stddev")[[1]]^2),
        R2.Residual=
        (attr(lme4::VarCorr(M0[[x]]), "sc")[[1]]^2 -
           attr(lme4::VarCorr(models[[x]]), "sc")^2)[[1]]/
          attr(lme4::VarCorr(M0[[x]]), "sc")[[1]]^2
        )

    } )

    R2 <- list(c("R2.intercept", round(random.variances.M0[1,],2)) ,
               c("R2.Residual", round(random.variances.M0[2,], 2)))

  # R2s<-sapply(3:ncol(random.variances), function(i) (random.variances[,i-1]-random.variances[,i]) / random.variances[,i-1] )
  # R2s<-lapply(1:nrow(R2s), function(x) c(paste("R2 - Reduction in variance of", random.variances$names[[x]]), NA, round(R2s[x,],2)  ))
  #

#New - finish it later
    # R2s<-sapply(models, function(x) {
    #   update(x,  update(formula.merMod(x), ".~ 1"))
    # }


  }




  if("fit" %in% fit.stats ) {
  fit <-sapply(models, function(x)  c(
              Deviance = comma(round(-2*logLik(x)[1], 0)),
              Nparameters = extractAIC(x)[1],
              AIC = comma(round(AIC(x),0)),
              BIC = comma(round(BIC(x),0)),
              Ngroups = ngrps(x)[[1]],
              Nobservations = comma(nobs(x)),
              Convergence = is.null(x@optinfo$conv$lme4$code)
              ))



  fit <-cbind(rownames(fit), fit)
  fit<-unname(unlist(apply(fit, 1, list), F))
  }

  ICC <- list(c("ICC",paste(lapply(models, myICC))))


  if("VIF" %in% fit.stats ) {
  VIF <- list(c("Max VIF",
                paste(lapply(models,
                             function(x) ifelse(length(fixef(x))>1,  round(max(vif_mer(x)),2), "-")))))
  }

  if("REML" %in% fit.stats ) {
  REML<- list(c("REML",   paste(lapply(models, function(y) summary(y)$devcomp$dims["REML"]!=0))))
  }


if("LRT" %in% fit.stats ) {
  LRT <- list(c("Likelihood ratio", NA, sapply(2:length(modelsML), function(i) {
    a<-anova(modelsML[[i]], modelsML[[i-1]])[2,]
    paste(round(a$Chisq,2), "(", a$`Chi Df`, ")", ifelse(a$`Pr(>Chisq)`<.001, "***",
                                                            ifelse(a$`Pr(>Chisq)`<.01, "**",
                                                                   ifelse(a$`Pr(>Chisq)`<.05, "*", ""))), sep="")
  })))
}

avalable.stats <- c("ICC", "VIF", "REML", "fit", "LRT", "random", "random.p",  "R2")

   if( sum(!fit.stats %in% avalable.stats)>0 )
   {

 warning(paste("I don't know stats ", paste(fit.stats[!fit.stats %in% avalable.stats], collapse=", "),
                   "; they are omitted.\n", sep=""))

    fit.stats <- fit.stats[fit.stats %in% avalable.stats]
   }


  extra.lines<- list()
  extra.lines<- unlist(lapply(fit.stats, function(x) append(extra.lines, get(x))), F)

  #print(random.variances)
  if(!silent) verb("Extraction of fit info took", Sys.time() - total.time)

  stage2.stargazer<-Sys.time()
  require("stargazer")
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")


  if(show.viewer==FALSE) {
    stargazer(models, out="good_table_output.html",
              type="html",
              summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),
                             #table.layout = "-l-d-m-c-t-a-",
                             omit.stat="all",
                             column.labels=mod.names,
                             add.lines=extra.lines,
                             digits=2, ...)
  } else {

    extra.lines<-lapply(extra.lines, function(x) gsub("_", "\\\\_", x))
    if(!silent)  print(extra.lines)
    #assign("extra.lines", extra.lines, envir=.GlobalEnv)
    #stargazer(hovs.fit1, type="html", out="123.html", add.lines=extra.lines, summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),    omit.stat="all")
    b<-capture.output(stargazer(models, out=htmlFile, type="html", summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),
                                #table.layout = "-l-d-m-c-t-a-",
                                omit.stat="all",
                                column.labels=mod.names,
                                add.lines=extra.lines,
                                digits=2,
                                notes=ifelse("random.p" %in% fit.stats,
                                             "# significant at p<.05 based on bootstrapped confidence intervals.",
                                             ""),
                                notes.append="random.p" %in% fit.stats,
                                ...
                                ))
  viewer	<- getOption("viewer")
  viewer	(htmlFile)
  }

  if(!silent) verb("stage2.stargazer", Sys.time()-stage2.stargazer)
  if(!silent) verb("Total time:", Sys.time()-total.time)
}


# Creates country level variables####
#' Creates country level variables by aggregating individual level variables
#'
#'
#'
#' @param ind_data A dataset with individual-level data.
#' @param ind_var Variable to be aggregated.
#' @param country_var Grouping variable.
#' @param FUN Aggregation function, mean by default.
#' @param suffix Character string, what should be added to the aggregated names of variables.
#'
#'
#' @export
#' @aliases aggr.and.merge
aggr_and_merge <- function(ind_data, ind_var, group_var, FUN="mean", suffix=NULL) {

  if(length(group_var)==1) {
  c.x<-with(ind_data, tapply(get(ind_var), get(group_var),
                             function(x) eval(call(FUN, x, na.rm=T)), simplify = T))
  c.y<-data.frame(c.x, grp=rownames(c.x))

  names(c.y)[1] <- paste(ind_var, ifelse(is.null(suffix), group_var, suffix),  sep=".")

  appended_data<-merge(x=ind_data, c.y, by.x=group_var, by.y="grp", all.x=TRUE)

  } else {
    aggs <- tapply(ind_data[,ind_var], ind_data[,group_var],
                   function(x) eval(call(FUN, x, na.rm=T)), simplify = T)

    aggs <-reshape2::melt(aggs)
    names(aggs)[names(aggs)=="value"]<-paste(ind_var, ifelse(is.null(suffix), paste0(group_var, collapse="."), suffix),  sep=".")
    appended_data<-merge(ind_data, aggs, by = group_var, all.x = T)
  }
  return(appended_data)
}

#' Computes pseudo-R2 by subtracting residual variances
#'
#'
#' @param base.model Base model
#' @param tested.model Test model
#'
#' Doesn't make use of Satorra-Bentler correction (should give this option in future).
#' @export
#' @aliases explained_variance, explained_variance.merMod
explained.variance.merMod <- function (base.model, tested.model) {

  base.model.vars   <-as.data.frame(lme4::VarCorr(base.model))
  tested.model.vars <-as.data.frame(lme4::VarCorr(tested.model))

  base.model.vars    <- base.model.vars[is.na(base.model.vars$var2),]
  tested.model.vars <- tested.model.vars[is.na(tested.model.vars$var2),]


  tabl<-  merge(base.model.vars[, 1:4], tested.model.vars[, 1:4], by=c("grp", "var1", "var2"), all=T, suffixes = c(".base",".tested"))
  tabl$explained <- with(tabl, (vcov.base-vcov.tested) /vcov.base)
  tabl
}

##Source: https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

#' Compute variance inflation factor for mer objects
#'
#' @param fit Object of class lmer.
#'
#' @details adapted from rms::vif
#'
#' @export
vif_mer <- function (fit) {
  v <- vcov(fit)
  nam <- names(fixef(fit))

  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  #print(v)
  #print(str(v))
  #class(v)<-"matrix"
  d <- Matrix::diag(v)^0.5
  v <- Matrix::diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# > vif.lme <- function (fit) {
#   >   ## adapted from rms::vif
#     >   v <- vcov(fit)
#     >   nam <- names(fixef(fit))
#     >   ## exclude intercepts
#       >   ns <- sum(nam == "Intercept" | nam == "(Intercept)")
#       >   if (ns > 0) v <- v[-(1:ns), -(1:ns), drop = FALSE]
#       >   diag(solve(cov2cor(v)))
#       > }



#' Add or remove terms to a lmer formula string
#'
#' @param lmer.formula Formula object or string containing formula.
#' @param fixed List of character strings containing variable names or terms to include in the formula as fixed terms.
#' @param random List of character strings containing variable names or terms to include in the formula as random terms.
#' @param drop List of character strings containing variable names or fixed terms to exclude from the formula.
#' @param drop.random List of character strings containing variable names or random terms to exclude from the formula.
#'
#' @examples f1 <- "a ~ b + c + (1| group)"
#' add_term(f1, , "Conservation")
#' add_term(f1, "Conservation*GDP")
#' add_term(f1, "Conservation*GDP", "Conservation")
#'
#' add_term(f1, drop="female", drop.random = "g.agea")
#'
#' @export

add_term <- function (lmer.formula, fixed=NULL, random=NULL, drop=NULL, drop.random=NULL) {

require(stringr)
require(stats)
require(lme4)

  if(class(lmer.formula)=="formula") lmer.formula<-Reduce(paste, deparse((as.formula(lmer.formula))))
  f<-paste(
    gsub("\\|", paste(ifelse(!is.null(random),  "+", ""),
                      paste(random, collapse="+"), "|"),
         lmer.formula),
    ifelse(!is.null(fixed),  "+", ""),
    paste(fixed, collapse = " + ")
  )

  if(!is.null(drop)) f<-update(as.formula(f), paste(".~.", paste("-", drop, collapse=" ")))
  if(!is.null(drop.random)) {
    if(class(f)=="formula") f<-Reduce(paste, deparse((as.formula(f))))
    #verb("f", f)
    rf<- grep("\\|", str_extract_all(f, "\\([^()]+\\)")[[1]], value = TRUE)
    #verb("rf", rf)
    rf.new<- gsub(drop.random, "", grep("\\|", str_extract_all(f, "\\([^()]+\\)")[[1]], value = TRUE))
    #verb("rf.new", rf.new)
    f<-sub(rf, rf.new, f, fixed=T)
  }

  if(class(f)=="formula") f<-Reduce(paste, deparse((as.formula(f))))
  f<-gsub("\\s+", " ",f)
  #verb("nospaces", f)
  f<-gsub("\\+\\s\\|", "\\|", f)
  f
}








# Potential interactions ####

#' Potential cross-level interactions
#'
#' Refits lmer model with given 'random terms' and correlates the predicted random effects with aggregated at group level variable mentioned at 'group.level.terms'
#'
#' @param random.terms Individual predictors  that should be made random and associated with predictors (mediators) at group level
#' @param group.level.terms Predictors (mediators) at the group level
#' @param lmer.fit Model fitted with lmer.
#' @param measure Character. correlation or interaction
#'
#' @export
potential_interactions <- function(random.terms, group.level.terms, lmer.fit, measure=c("correlation", "interaction")) {

  measure <- measure[1]


  # CORRELATIONS
  if(measure=="correlation") {

    data<-eval(parse(text=deparse(lmer.fit@call$data)))

    #f.part1 <- gsub("\\(Intercept\\)", "1", paste(paste(dimnames(model.matrix(lmer.fit))[[2]], collapse=" + "), "+(",
    #paste(names(ranef(lmer.fit)[[1]]), collapse=" + "), "+"))
    #f.part2 <- paste("|", names(getME(lmer.fit, "flist")), ")")

    re<-sapply(random.terms, function(x) ranef(update(lmer.fit, add_term(lmer.fit@call$formula, random=x))   )[[1]][,x] )

    iv <- aggregate(data[,group.level.terms], list(data[,names(getME(lmer.fit, "flist"))]), mean, na.rm=T)

    # out <- round(cor(cbind(re, iv[,-1]), use="pairwise.complete.obs")[1:length(random.terms),
    #                                                                   (length(random.terms)+1):(length(random.terms)+length(group.level.terms))],
    #              3)

    out<-cor_table(cbind(re, iv[,-1]))[1:length(random.terms),
                                       (length(random.terms)+1):(length(random.terms)+length(group.level.terms))]

    cat("\nCorrelations \n")
    print(out, row.names=T)
    df_to_viewer(out)

    # INTERACTIONS
  } else if(measure=="interaction") {

    pairs <- expand.grid(random.terms, group.level.terms, stringsAsFactors = F)

    formulas <-  apply(pairs, 1, function(x) add_term(lmer.fit@call$formula, paste(x, collapse="*"),
                                                      random= x[1]   ))

    tb <- lapply(1:length(formulas), function(x)  {

      new.fit <- update(lmer.fit, formulas[x])

      td <- data.frame(est=fixef(new.fit), se= sqrt(Matrix::diag(vcov.merMod(new.fit))))

      td <-td[c(pairs[x,1], pairs[x,2], paste(pairs[x,1], pairs[x,2], sep =":")),]

      td$t <- td$est/td$se

      a<- apply(td, 1, function(z) paste(round(z["est"], 2),
                                         "(", round(z["se"], 2), ")",
                                         ifelse( abs(z["t"])<1.96, "", ifelse(abs(z["t"])<2.9, "*", ifelse(abs(z["t"])<5, "**", "***"))),
                                         sep=""
      )
      )
       print(a)
      a

    })


    b<- data.frame(pairs,
                   value=apply(pairs, 1, function(p)  unlist(tb)[names(unlist(tb))== paste(p, collapse=":")]), stringsAsFactors = F)

    cat("\nInteractions \n")
    print(reshape2::dcast(b, Var1 ~ Var2), row.names=F)
    invisible(c)
    df_to_viewer(b)

  } else {
    warning("Argument measure should be either 'correlation' or 'interaction'")
  }



}

#' Potential individual-level interactions for lmer model
#'
#' @description  Takes a list of variables and computes models with all possible interactions between them, one by one.
#'
#' @param variables Character list of variable names in the model which are already fixed effects.
#' @param modelfit lmer model
#'
#' @return Returns a data frame of class "LHinteractions" supplied with a corresponding print method.
#'
#' @export
potential_interactions_ind <- function(variables, modelfit) {

  trms <- apply(combn(variables, 2), 2, function(x) paste(x[1], x[2], sep=":"))
  ft<-lapply(trms, function(x) {
    update(modelfit, add_term(modelfit@call$formula, fixed=x))
  })

  intr.eff <- sapply(1:length(ft), function(m) {
    verb(trms[m])
    a<-summary(ft[[m]])
    a$coefficients[trms[m],]
  })
  intr.eff <-t(intr.eff)
  rownames(intr.eff)<-trms
  intr.eff<-as.data.frame.matrix(intr.eff)
  intr.eff$star <- sapply(intr.eff[,"t value"], function(x) ifelse(abs(x)>2.58, "***",
                                                                   ifelse(abs(x)>2.33 , "**",
                                                                          ifelse(abs(x)>1.96, "*", ""))))
  #print(format(intr.eff, digits=1, nsmall=1, scientific =FALSE))
  intr.eff$v1 <- sapply(strsplit(rownames(intr.eff), ":"), `[`, 1)
  intr.eff$v2 <- sapply(strsplit(rownames(intr.eff), ":"), `[`, 2)
  intr.eff$vlu <- paste(round(intr.eff$Estimate,2), intr.eff$star, sep="")
  lvls<-unique(c(intr.eff$v1,intr.eff$v2))

  intr.eff$v2 <-factor(intr.eff$v2, levels=lvls)
  intr.eff$v1 <-factor(intr.eff$v1, levels=lvls)

  print(reshape2::dcast(intr.eff, v2 ~ v1, value.var ="vlu", fill = ""))
  #class(intr.eff)<-c("LHinteractions", "data.frame")
  #print.LHinteractions(intr.eff)
  invisible(intr.eff)
}


# Method for a nice printing of LHinteractions class
#
#
# @export
# print.LHinteractions <- function(x) print(reshape2::dcast(x, v2 ~ v1, value.var ="vlu", fill = ""))

# Centering ####

#' Group-centering of one or more variables
#'
#'
#' @export
group_center <- function(variables, group, data, prefix="g.") {
  new.data <- data[,c(group,variables)]
  # l<- sapply(unique(data[,group]),
  #            function(g) sapply(variables,
  #                               function(v)  {
  #                                 new.data[new.data[,group]==g, v]<- data[data[,group]==g, v] - mean(data[data[,group]==g, v], na.rm=T)
  #                               })  )
  # names(new.data) <- paste(prefix, names(new.data), sep="")
  # new.data[, -1]

  # new <- merge(data[,c(group, variables)],
  #              aggregate(as.data.frame(data[,variables]), list(data[,group]), mean, na.rm=T),
  #              by.x=group, by.y="Group.1", all.x=T, suffixes = c("", ".a"))


  for(v in variables)  {
    ag<- tapply(new.data[,v], list(new.data[,group]), mean, na.rm = T)
    for(g in unique(new.data[,group])) new.data[new.data[,group]==g, v]<- new.data[new.data[,group]==g, v] -ag[g]
  }
  new.data[,group]<-NULL
  names(new.data)<-paste(prefix, variables, sep="")

  # for(x in variables) {
  #   new$new <- rep(NA, nrow(new))
  #   new$new <- new[,x] - new[,paste(x,"a", sep=".")]
  #   names(new)[length(new)]<-paste(prefix, x, sep="")
  #   new[,paste(x,"a", sep=".")]<-NULL
  #   new[,x]<-NULL
  # }
  # new[,group]<-NULL
  cbind(data, new.data)


}

#' Grand mean centering of one or more variables
#'
#'
#' @export
grand_center <- function(variables, prefix="gm.") {

  new.data <- as.data.frame(sapply(variables, function(x) x-mean(x, na.rm=T)))

  names(new.data) <- paste(prefix, names(new.data), sep="")

  new.data
}

#Corr by country#####
#' Within-group correlations
#'
#' @param var1 String. Name of variable to correlate.
#' @param var2 String. Name of variable to correlate.
#' @param group Grouping variable.
#' @param data Data.framer containing var1, var2, and group.
#' @param plot Logical. Should the plot be created?
#' @param labs Logical. Should value labels be used?
#'
#' Prints correlations in console, and plots in a graph.
#' @export
cor_within <- function (var1, var2, group, data, plot=TRUE, labs=TRUE, use="pairwise", ...) {

  #require(sjmisc);
  #require(sjlabelled);
  library("ggplot2")


  if(sum(class(data)=="tbl")>0) {
    Gr <- as.character(lab_to_fac(data[, group][[1]]))
    V1 <- as.numeric(data[, var1][[1]])
    V2 <- as.numeric(data[, var2][[1]])
  } else {
    Gr <- as.character(lab_to_fac(data[, group]))
    V1 <- data[, var1]
    V2 <- data[, var2]
  }

  tb<-data.frame(group=unique(Gr),
                 Corr=sapply(unique(Gr),   function(x) {
                   cor(V1[Gr==x], V2[Gr==x], use=use, ...)
                   # b<-cor.test(V1[Gr==x], V2[Gr==x], use="complete.obs")
                   # c(b[["estimate"]], b[["p.value"]])
                 },
                 simplify=T),
                 stringsAsFactors = F)

  tb$group<-factor(tb$group, levels=tb$group[order(tb$Corr)])
  if(plot==T) {
    g<-ggplot(tb,aes(x=group, y=Corr))+
      geom_point(colour="red")+
      coord_flip()+theme_minimal()
    if(labs) g<-g+geom_text_repel(aes(label=(round(Corr, 2))), size=3)

    print(g)
  }
  return(tb)

}


#' Correlation between group aggregates
#'
#'
#'@param var1 Character name of variable 1, or a list of characters (in this case var2 is ignored)
#'@param var2 Character name of variable 2.
#'@param group Character name of group variable.
#'@param data Dataset
#'@param ... Passed to `cor.test`
#'
#' @export

cor_between <- function (var1, var2, group, data, print = T, ...) {
  if(any(class(data)=="tbl")) data<-drop_labs(data);

  if(length(var1)==1) {
    var1 = data[,var1]
    var2 = data[,var2]
    group =data[,group]



    dt<-data.frame(

      mean1=tapply(var1, sjmisc::to_label(group), function(x) mean(x, na.rm=T), simplify = T),
      mean2=tapply(var2, sjmisc::to_label(group), function(x) mean(x, na.rm=T), simplify = T)
    )

    cr <- cor.test(dt$mean1, dt$mean2, na.action="na.omit", method = "pearson", ...)

    out <- list(
      cor=cr$estimate,
      n=nrow(na.omit(cbind(dt$mean1, dt$mean2))),
      p.value=cr$p.value
    )

    if(print == T) {
    print(data.frame(Correlation=round(out$cor, 2),
                     n= out$n,
                     p.value=round(out$p.value, 3),
                     `.`= ifelse(out$p.value<0.001, "***", ifelse(out$p.value<0.01, "**", ifelse(out$p.value<0.05, "*", "")))   ),
          row.names = F)
    }

    invisible(out)


  }  else {
if(print == T) {
    print(cor_table(aggregate(data[,var1], list(data[,group]), mean, na.rm=T)[,-1], method="pearson", star=TRUE))
}
    invisible(cor_table(
      aggregate(data[,var1], list(data[,group]), mean, na.rm=T)[,-1], method="pearson", star=FALSE))
  }
}

# What fixed should be turned to random ####
#' Find random effects across fixed
#'
#'
#'
#' @return
#'
#'@export
search_random <- function(lmerfit, terms=NA, boot=F) {
  if(any(is.na(terms))) {
    terms <- dimnames(getME(lmerfit, "X"))[[2]]
    terms<-terms[terms!="(Intercept)"]
  }

  o <-  lapply(terms, function(i) {
    verb("Fitting model with random", i)
    update(lmerfit, add_term(lmerfit@call$formula, random=i))
  })

  lapply(1:length(o), function(x) VarCorr(o[[x]])[[names(lmerfit@flist)]][terms[[x]], terms[[x]]])

  #anova(lmerfit)

  #confint(lmerfit, method="Wald")
}


# Rights, J.D., & Sterba, S.K. (in press). Quantifying explained variance in multilevel models: An integrative framework for defining R-squared measures. Psychological Methods.
# r2MLM <- function(data,within_covs,between_covs,random_covs,
#                   gamma_w,gamma_b,Tau,sigma2,has_intercept=T,clustermeancentered=T){
#   if(has_intercept==T){
#     if(length(gamma_b)>1) gamma <- c(1,gamma_w,gamma_b[2:length(gamma_b)])
#     if(length(gamma_b)==1) gamma <- c(1,gamma_w)
#     if(is.null(within_covs)==T) gamma_w <- 0
#   }
#   if(has_intercept==F){
#     gamma <- c(gamma_w,gamma_b)
#     if(is.null(within_covs)==T) gamma_w <- 0
#     if(is.null(between_covs)==T) gamma_b <- 0
#   }
#   if(is.null(gamma)) gamma <- 0
#   ##compute phi
#   phi <- var(cbind(1,data[,c(within_covs)],data[,c(between_covs)]),na.rm=T)
#   if(has_intercept==F) phi <- var(cbind(data[,c(within_covs)],data[,c(between_covs)]),na.rm=T)
#   if(is.null(within_covs)==T & is.null(within_covs)==T & has_intercept==F) phi <- 0
#   phi_w <- var(data[,within_covs],na.rm=T)
#   if(is.null(within_covs)==T) phi_w <- 0
#   phi_b <- var(cbind(1,data[,between_covs]),na.rm=T)
#   if(is.null(between_covs)==T) phi_b <- 0
#   ##compute psi and kappa
#   var_randomcovs <- var(cbind(1,data[,c(random_covs)]),na.rm=T)
#   if(length(Tau)>1) psi <- matrix(c(diag(Tau)),ncol=1)
#   if(length(Tau)==1) psi <- Tau
#   if(length(Tau)>1) kappa <- matrix(c(Tau[lower.tri(Tau)==TRUE]),ncol=1)
#   if(length(Tau)==1) kappa <- 0
#   v <- matrix(c(diag(var_randomcovs)),ncol=1)
#   r <- matrix(c(var_randomcovs[lower.tri(var_randomcovs)==TRUE]),ncol=1)
#   if(is.null(random_covs)==TRUE){
#     v <- 0
#     r <- 0
#     m <- matrix(1,ncol=1)
#   }
#   if(length(random_covs)>0) m <- matrix(c(colMeans(cbind(1,data[,c(random_covs)]),na.rm=T)),ncol=1)
#   ##total variance
#   totalvar_notdecomp <- t(v)%*%psi + 2*(t(r)%*%kappa) + t(gamma)%*%phi%*%gamma + t(m)%*%Tau%*%m + sigma2
#   totalwithinvar <- (t(gamma_w)%*%phi_w%*%gamma_w) + (t(v)%*%psi + 2*(t(r)%*%kappa)) + sigma2
#   totalbetweenvar <- (t(gamma_b)%*%phi_b%*%gamma_b) + Tau[1]
#   totalvar <- totalwithinvar + totalbetweenvar
#   ##total decomp
#   decomp_fixed_notdecomp <- (t(gamma)%*%phi%*%gamma) / totalvar
#   decomp_fixed_within <- (t(gamma_w)%*%phi_w%*%gamma_w) / totalvar
#   decomp_fixed_between <- (t(gamma_b)%*%phi_b%*%gamma_b) / totalvar
#   decomp_fixed <- decomp_fixed_within + decomp_fixed_between
#   decomp_varslopes <- (t(v)%*%psi + 2*(t(r)%*%kappa)) / totalvar
#   decomp_varmeans <- (t(m)%*%Tau%*%m) / totalvar
#   decomp_sigma <- sigma2/totalvar
#   ##within decomp
#   decomp_fixed_within_w <- (t(gamma_w)%*%phi_w%*%gamma_w) / totalwithinvar
#   decomp_varslopes_w <- (t(v)%*%psi + 2*(t(r)%*%kappa)) / totalwithinvar
#   decomp_sigma_w <- sigma2/totalwithinvar
#   ##between decomp
#   decomp_fixed_between_b <- (t(gamma_b)%*%phi_b%*%gamma_b) / totalbetweenvar
#   decomp_varmeans_b <- Tau[1] / totalbetweenvar
#   #NEW measures
#   if (clustermeancentered==TRUE){
#     R2_f <- decomp_fixed
#     R2_f1 <- decomp_fixed_within
#     R2_f2 <- decomp_fixed_between
#     R2_fv <- decomp_fixed + decomp_varslopes
#     R2_fvm <- decomp_fixed + decomp_varslopes + decomp_varmeans
#     R2_v <- decomp_varslopes
#     R2_m <- decomp_varmeans
#     R2_f_w <- decomp_fixed_within_w
#     R2_f_b <- decomp_fixed_between_b
#     R2_fv_w <- decomp_fixed_within_w + decomp_varslopes_w
#     R2_v_w <- decomp_varslopes_w
#     R2_m_b <- decomp_varmeans_b
#   }
#   if (clustermeancentered==FALSE){
#     R2_f <- decomp_fixed_notdecomp
#     R2_fv <- decomp_fixed_notdecomp + decomp_varslopes
#     R2_fvm <- decomp_fixed_notdecomp + decomp_varslopes + decomp_varmeans
#     R2_v <- decomp_varslopes
#     R2_m <- decomp_varmeans
#   }
#   if(clustermeancentered==TRUE){
#     decomp_table <- matrix(c(decomp_fixed_within,decomp_fixed_between,decomp_varslopes,decomp_varmeans,decomp_sigma,
#                              decomp_fixed_within_w,"NA",decomp_varslopes_w,"NA",decomp_sigma_w,
#                              "NA",decomp_fixed_between_b,"NA",decomp_varmeans_b,"NA"),ncol=3)
#     rownames(decomp_table) <- c("fixed, within","fixed, between","slope variation","mean variation","sigma2")
#     colnames(decomp_table) <- c("total","within","between")
#     R2_table <- matrix(c(R2_f1,R2_f2,R2_v,R2_m,R2_f,R2_fv,R2_fvm,
#                          R2_f_w,"NA",R2_v_w,"NA","NA",R2_fv_w,"NA",
#                          "NA",R2_f_b,"NA",R2_m_b,"NA","NA","NA")
#                        ,ncol=3)
#     rownames(R2_table) <- c("f1","f2","v","m","f","fv","fvm")
#     colnames(R2_table) <- c("total","within","between")
#   }
#   ##barchart
#   if(clustermeancentered==TRUE){
#     contributions_stacked <- matrix(c(decomp_fixed_within,decomp_fixed_between,decomp_varslopes,decomp_varmeans,decomp_sigma,
#                                       decomp_fixed_within_w,0,decomp_varslopes_w,0,decomp_sigma_w,
#                                       0,decomp_fixed_between_b,0,decomp_varmeans_b,0),5,3)
#     colnames(contributions_stacked) <- c("total","within","between")
#     rownames(contributions_stacked) <- c("fixed slopes (within)",
#                                          "fixed slopes (between)",
#                                          "slope variation (within)",
#                                          "intercept variation (between)",
#                                          "residual (within)")
#     barplot(contributions_stacked, main="Decomposition", horiz=FALSE,
#             ylim=c(0,1),col=c("darkred","steelblue","darkred","midnightblue","white"),ylab="proportion of variance",
#             density=c(NA,NA,30,40,NA),angle=c(0,45,0,135,0),xlim=c(0,1),width=c(.3,.3))
#     legend(.30,-.1,legend=rownames(contributions_stacked),fill=c("darkred","steelblue","darkred","midnightblue","white"),
#            cex=.7, pt.cex = 1,xpd=T,density=c(NA,NA,30,40,NA),angle=c(0,45,0,135,0))
#   }
#   if(clustermeancentered==FALSE){
#     decomp_table <- matrix(c(decomp_fixed_notdecomp,decomp_varslopes,decomp_varmeans,decomp_sigma),ncol=1)
#     rownames(decomp_table) <- c("fixed","slope variation","mean variation","sigma2")
#     colnames(decomp_table) <- c("total")
#     R2_table <- matrix(c(R2_f,R2_v,R2_m,R2_fv,R2_fvm),ncol=1)
#     rownames(R2_table) <- c("f","v","m","fv","fvm")
#     colnames(R2_table) <- c("total")
#     ##barchar
#     contributions_stacked <- matrix(c(decomp_fixed_notdecomp,decomp_varslopes,decomp_varmeans,decomp_sigma),4,1)
#     colnames(contributions_stacked) <- c("total")
#     rownames(contributions_stacked) <- c("fixed slopes",
#                                          "slope variation",
#                                          "intercept variation",
#                                          "residual")
#     barplot(contributions_stacked, main="Decomposition", horiz=FALSE,
#             ylim=c(0,1),col=c("darkblue","darkblue","darkblue","white"),ylab="proportion of variance",
#             density=c(NA,30,40,NA),angle=c(0,0,135,0),xlim=c(0,1),width=c(.6))
#     legend(.30,-.1,legend=rownames(contributions_stacked),fill=c("darkblue","darkblue","darkblue","white"),
#            cex=.7, pt.cex = 1,xpd=TRUE,density=c(NA,30,40,NA),angle=c(0,0,135,0))
#   }
#   Output <- list(noquote(decomp_table),noquote(R2_table))
#   names(Output) <- c("Decompositions","R2s")
#   return(Output)
# }
#
#
# ###example input
#
# data <- matrix(NA,100,4)
# xs <- mvrnorm(n=100,mu=c(0,0),Sigma=matrix(c(2,.75,.75,1.5),2,2))
# ws <- mvrnorm(n=10,mu=c(0,2),Sigma=matrix(c(1,.5,.5,2),2,2))
# data[,1:2] <- xs
# for (i in seq(10)){
#   data[(10*(i-1)+1):(i*10),3] <- ws[i,1]
#   data[(10*(i-1)+1):(i*10),4] <- ws[i,2]
#   data[(10*(i-1)+1):(i*10),1] <- data[(10*(i-1)+1):(i*10),1] - mean(data[(10*(i-1)+1):(i*10),1])
#   data[(10*(i-1)+1):(i*10),2] <- data[(10*(i-1)+1):(i*10),2] - mean(data[(10*(i-1)+1):(i*10),2])
# }
# r2MLM(data,within_covs=c(1,2),between_covs=c(3,4),random_covs=c(1,2),
#       gamma_w=c(2.5,-1),gamma_b=c(1,.25,1.5),Tau=matrix(c(4,1,.75,1,1,.25,.75,.25,.5),3,3),sigma2=10)



#' mplus skeleton
#'
#' @param d2 Data frame to extract variable names
#'
#' @example cat(mplus_skeleton(cars), file = "mplus1.inp")
#' @export
mplus_skeleton<- function(d2) {
  var.names<- paste(gsub("\\.", "_", abbreviate(names(d2), 8)), " ! ", names(d2), "\n", collapse = " ")
  if(sum(duplicated(gsub("\\.", "_", abbreviate(names(d2), 8))))>0) warning("Some abbreviated variable names are duplicated!!")
  paste0(c("DATA:","\n",
           "   file = 'mplus_temp.tab';", "\n",
           " VARIABLE:", "\n",
           "   names =",

           paste(var.names),


           ";\n",
           "   missing = .;", "\n",
           "   usevariables = ",
           "\n\n\n\n",
           "ANALYSIS:\n",
           "  type = twolevel random;\n",
           "  estimator = mlr;\n\n\n\n",

           "MODEL:\n\n\n\n",

           "OUTPUT:  tech6;"

  ), collapse = "")
}

#' Function producing a good table for many ML regressions #####
#'
#' @param models List of the fitted lmer objects
#' @param fit.stats What fit statistics to compute/extract and show? Possible options are "ICC", "random", "R2s", "fit", "LRT", "REML", "VIF".
#' @param mod.names Vector of the same length as models list, giving names to each model.
#' @param show.viewer Logical. Whether the resulting table should be shown in the RStudio viewer. If FALSE then the file "good_table_output.html" is saved to working directory.
#' @param ... Arguments passed to stargazer.
#'
#'
#'@examples
#'#m1<- lmer(HE ~ age + gender + (1|country), ess.data.d, weights=dweight)
#'#m2<- lmer(HE ~ age + gender + (1+age + gender|country), ess.data.d, weights=dweight)
#'#good_table(list(m1, m2))
#'
#' # Another option
#' #f1 <- as.formula(HE ~ age + gender + (1|country))
#' #f2 <- as.formula(HE ~ age + gender + (1|country))
#' #good_table(lapply(c(f1, f2), function(x) lmer(x, ess.data.d, weights=dweight)))
#'
#'@note Issues to implement:
#' * Might be veeery slow (something to work on)
#' * Add row indication of non-convergence
#'
#' @md
#'
#' @export
good_table <- function(models,
                       fit.stats=c("ICC", "random", "random.p", "R2", "fit", "LRT", "REML", "VIF"),
                       mod.names="",
                       show.viewer=TRUE,
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
    if(any(sapply(models, isREML))) {
      message("Refitting model with ML estimator to extract correct deviance and compute information criteria. \n Might take a long time.")
    pb<-txtProgressBar(0, length(models), label="Going progress bar...", style = 3)
    modelsML<- lapply(1:length(models), function(m) {
        refitML(models[[m]])
        utils::setTxtProgressBar(pb, m)
    })
    } else {
      modelsML <- models
    }

  }



  if("random" %in% fit.stats | "random.p" %in% fit.stats ) {

  random.variances<-lapply(models, function(x) {
    variances<-c(attr(lme4::VarCorr(x)[[1]], "stddev")^2, Residual=attr(lme4::VarCorr(x), "sc")^2 )
    data.frame(variances, names=paste("Variance of", names(variances)), stringsAsFactors = F)
                      } )
  random.variances<-suppressWarnings(Reduce(function(x,y) merge(x,y, by="names", all =T, suffixes=letters), random.variances))

  verb("random.variances"); print(random.variances)

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
  fit <-sapply(modelsML, function(x)  c(
              Deviance = comma(round(deviance(x,REML=F), 0)),
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
  verb("Extraction of fit info took", Sys.time() - total.time)

  stage2.stargazer<-Sys.time()
  require("stargazer")
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")


  if(show.viewer==FALSE) {
    stargazer(models, out="good_table_output.html", type="html", summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),
                             #table.layout = "-l-d-m-c-t-a-",
                             omit.stat="all",
                             column.labels=mod.names,
                             add.lines=extra.lines,
                             digits=2, ...)
  } else {

    extra.lines<-lapply(extra.lines, function(x) gsub("_", "\\\\_", x))
    print(extra.lines)
    assign("extra.lines", extra.lines, envir=.GlobalEnv)
    #stargazer(hovs.fit1, type="html", out="123.html", add.lines=extra.lines, summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),    omit.stat="all")
    b<-capture.output(stargazer(models, out=htmlFile, type="html", summary=F, no.space=T, single.row=T, star.cutoffs=c(0.05, 0.01, 0.001),
                                #table.layout = "-l-d-m-c-t-a-",
                                omit.stat="all",
                                #column.labels=mod.names,
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

  verb("stage2.stargazer", Sys.time()-stage2.stargazer)
  verb("Total time:", Sys.time()-total.time)
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
aggr_and_merge <- function(ind_data, ind_var, country_var, FUN="mean", suffix=NULL) {
  c.x<-with(ind_data, tapply(get(ind_var), get(country_var),
                             function(x) eval(call(FUN, x, na.rm=T)), simplify = T))
  c.y<-data.frame(c.x, grp=rownames(c.x))

  names(c.y)[1] <- paste(ind_var, ifelse(is.null(suffix), country_var, suffix),  sep=".")

  appended_data<-merge(x=ind_data, c.y, by.x=country_var, by.y="grp", all.x=TRUE)
  appended_data
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

#


#' Poterntial interactions
#'
#' Refits lmer model with given 'random terms' and correlates the predicted random effects with aggregated at group level variable mentioned at 'group.level.terms'
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

out <- round(cor(cbind(re, iv[,-1]))[1:length(random.terms),
                                     (length(random.terms)+1):(length(random.terms)+length(group.level.terms))],
             3)

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
    c<- reshape2::dcast(b, Var1 ~ Var2)
    out <- list(tb, c)

  } else {
    warning("Argument measure should be either 'correlation' or 'interaction'")
  }

  out

}

#' Group-centering of one or more variables
#'
#'
#' @export
group_center <- function(variables, group, data, prefix="g.") {
  new.data <- data[,c(group,variables)]
  l<- sapply(unique(data[,group]),
             function(g) sapply(variables,
                                function(v)  {
                                  new.data[new.data[,group]==g, v]<- data[data[,group]==g, v] - mean(data[data[,group]==g, v], na.rm=T)
                                })  )
  names(new.data) <- paste(prefix, names(new.data), sep="")
  new.data[, -1]
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





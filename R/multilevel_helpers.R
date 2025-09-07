#' Create Regression Table for Multilevel Models
#'
#' Produces a formatted table for multiple lmer fitted objects with various
#' fit statistics and model comparisons.
#'
#' @param models List of fitted lmer objects
#' @param fit_stats Character vector of statistics to include. Options: "ICC",
#'   "VIF", "REML", "fit", "LRT", "random", "random_p", "random_ci", "random_anova", "R2". See details.
#' @param model_names Character vector of model names (default: auto-generated)
#' @param show_viewer Logical. Show in RStudio viewer (TRUE) or go with stargazer's default (which is latex, see `type` argument in \code{\link[stargazer]{stargazer}}
#' @param silent Logical. Suppress progress messages
#' @param digits Integer. Number of digits for rounding
#' @param ... Additional arguments passed to \code{\link[stargazer]{stargazer}}
#'
#' @return Invisibly returns the HTML output path
#'
#' @details
#' Fit statistics:
#' \describe{
#'   \item{fit}{Deviance, AIC, BIC, parameters, groups, observations, convergence}
#'   \item{ICC}{Intra-class correlation coefficient}
#'   \item{R2}{R-squared from empty model comparison}
#'   \item{LRT}{Likelihood ratio tests between nested models}
#'   \item{random}{Random effect variances}
#'   \item{random_p}{Random effect variances with bootstrapped significance sign}
#'   \item{random_ci}{Random effect variances with bootstrapped confidence intervals and sig sign}
#'   \item{VIF}{Maximum variance inflation factor}
#'   \item{REML}{Whether REML estimation was used}
#' }
#'
#' @importFrom lme4 VarCorr ngrps isREML refitML getME fixef
#' @importFrom stats logLik AIC BIC nobs anova update
#' @importFrom scales comma percent
#' @importFrom stargazer stargazer
#' @importFrom rstudioapi viewer
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @examples
#' \dontrun{
#' data("sleepstudy", package = "lme4")
#' m1 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
#' m2 <- lmer(Reaction ~ Days + (1+Days|Subject), sleepstudy)
#' lmer_table(list(m1, m2))
#' }
#'
#' @export
lmer_table <- function(models,
                       fit_stats = c("fit", "random"),
                       model_names = NULL,
                       show_viewer = TRUE,
                       silent = TRUE,
                       digits = 2,
                       ...) {

  # Input validation
  available_stats <- c("ICC", "VIF", "REML", "fit", "LRT", "random",
                       "random_p", "random_ci", "random_anova", "R2")

  if(any(fit_stats=="all")) fit_stats <- available_stats[-c(7,8)]

  validate_inputs(models, fit_stats, model_names, available_stats)

  # Setup
  start_time <- Sys.time()
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_along(models))
  }

  # Calculate all requested statistics
  extra_lines <- calculate_fit_statistics(models, fit_stats, silent)

  # Generate table
  html_path <- generate_stargazer_table(
    models, extra_lines, model_names, show_viewer, digits, silent, ...
  )

  if (!silent) {
    message("Total time: ", format(Sys.time() - start_time, digits = 2))
  }

  invisible(html_path)
}

# Input validation helper
validate_inputs <- function(models, fit_stats, model_names, available_stats) {
  # Check models
  if (!is.list(models) || length(models) == 0) {
    stop("'models' must be a non-empty list")
  }

  if (!all(sapply(models, function(x) inherits(x, "merMod")))) {
    stop("All elements in 'models' must be lmer/glmer objects")
  }

  # Check fit statistics

  unknown_stats <- setdiff(fit_stats, available_stats)
  if (length(unknown_stats) > 0) {
    warning("Unknown fit statistics ignored: ", paste(unknown_stats, collapse = ", "))
  }

  # Check model names
  if (!is.null(model_names) && length(model_names) != length(models)) {
    stop("'model_names' must have same length as 'models'")
  }

  # Check sample consistency for LRT
  if ("LRT" %in% fit_stats) {
    n_obs <- sapply(models, stats::nobs)
    if (!all(n_obs == n_obs[1])) {
      stop("All models must be fit to the same sample for LRT computation")
    }
  }
}


# Main statistics calculation dispatcher
calculate_fit_statistics <- function(models, fit_stats, silent) {
  # Remove conflicts (random_p supersedes random)
  if (sum(c("random", "random_p", "random_ci") %in% fit_stats)>1) {
    fit_stats <- setdiff(fit_stats, c( "random", "random_ci"))
    if (!silent) message("Using 'random_p' instead of 'random'")
  }

  # Define computation functions for each statistic
  compute_stat <- function(stat) {
    if (!silent) message("Computing ", stat, "...")

    switch(stat,
           "fit" = {
             fit_matrix <- sapply(models, function(model) {
               c(
                 "Deviance" = scales::comma(round(-2 * stats::logLik(model)[1], 0)),
                 "Parameters" = attr(stats::logLik(model), "df"),
                 "AIC" = scales::comma(round(stats::AIC(model), 0)),
                 "BIC" = scales::comma(round(stats::BIC(model), 0)),
                 "Groups" = lme4::ngrps(model)[[1]],
                 "Observations" = scales::comma(stats::nobs(model)),
                 "Converged" = check_convergence(model)
               )
             })
             # Convert each row to a list element: c(row_name, values_for_each_model)
             lapply(rownames(fit_matrix), function(row_name) {
               c(row_name, fit_matrix[row_name, ])
             })
           },
           "ICC" =  list(c("ICC", compute_icc(models))),
           "VIF" = list(c("VIF", sapply(models, vif_mer))),
           "REML" = list(c("REML", lapply(models, function(m) lme4::isREML(m)))),
           "LRT" = list(c("LRT", compute_lrt(models, silent))),
           "random" = compute_random_effects(models),
           "random_p" = compute_random_effects_with_ci(models),
           "random_ci" = compute_random_effects_with_ci(models, ci = T),
           "random_anova" = compute_random_anova(models),
           "R2" = {
             r2_results <- compute_r2(models)
             #lapply(r2_results, function(row) c("R2", row))
             lapply(rownames(r2_results), function(row_name) {
               c(row_name, round(r2_results[row_name, ], 2))
             })
           }
    )
  }

  # Apply computation to all requested statistics and flatten the results
  result_list <- unlist(lapply(fit_stats, compute_stat), recursive = FALSE)


  return(result_list)
}




compute_icc <- function(models) {
  icc_values <- sapply(models, function(model) {
    variances <- as.data.frame(lme4::VarCorr(model))$vcov
    icc <- variances[1] / sum(variances[c(1, length(variances))])
    paste0(round(icc * 100, 1), "%")
  })

  icc_values
}



compute_lrt <- function(models, silent) {
  # Refit with ML if necessary
  if (any(sapply(models, lme4::isREML))) {
    if (!silent) {
      message("Refitting models with ML for LRT...")
      pb <- utils::txtProgressBar(0, length(models), style = 3)
    }

    models_ml <- lapply(seq_along(models), function(i) {
      result <- lme4::refitML(models[[i]])
      if (!silent) utils::setTxtProgressBar(pb, i)
      result
    })

    if (!silent) close(pb)
  } else {
    models_ml <- models
  }

  # Compute likelihood ratio tests
  lrt_results <- sapply(2:length(models_ml), function(i) {
    anova_result <- stats::anova(models_ml[[i]], models_ml[[i-1]])[2, ]
    chisq <- f(anova_result$Chisq, 2)
    df <- anova_result$`Df`
    p_val <- anova_result$`Pr(>Chisq)`
    stars <- pvalue_to_stars(p_val)

    paste0(chisq, " (", df, ")", stars)
  })

  c(NA, lrt_results)
}

compute_random_effects <- function(models, df = F) {
  # Extract variance components
  variance_list <- lapply(models, function(model) {
    var_corr <- lme4::VarCorr(model)
    variances <- c(
      attr(var_corr[[1]], "stddev")^2,
      Residual = attr(var_corr, "sc")^2
    )
    data.frame(
      variances = variances,
      names = paste("Variance of", names(variances)),
      stringsAsFactors = FALSE
    )
  })

  # Merge all variance components
  merged_variances <- Reduce(function(x, y) {
    merge(x, y, by = "names", all = TRUE, suffixes = paste0("_", seq_along(models)))
  }, variance_list)

  # Format for output
  if(!df) {
  lapply(seq_len(nrow(merged_variances)), function(i) {
    c(merged_variances$names[i],
      #format(unlist(merged_variances[i, -1]), digits = 3, scientific = FALSE)
      f(unlist(merged_variances[i, -1]),2)
      )
  })
  } else {
    return(merged_variances)
  }
}

compute_random_effects_with_ci <- function(models, ci = F) {


  message(
    "Computing bootstrapped p-values. Usually it takes a long time. Be patient, or interrupt and use fit.stats='random' instead of 'random.p'. "
  )
  boot.models <- lapply(models, function(x) {
    confint.merMod(
      x,
      nsim = 100,
      parm = "theta_",
      boot.type = "basic",
      method = "boot",
      parallel = "multicore",
      ncpus = 4,
      oldNames = F
    )
  })


random.variances <- compute_random_effects(models, df= T)

p.ci = lapply(boot.models, function(x) apply(x, 1, function(y) {
    ci.str = paste0(f(y[1]^2,1), " - ", f(y[2]^2,1))
    sig = ifelse(y[1] * y[2] > 0, "#", "ns")
    if(ci) paste0(sig, " [", ci.str, "]") else sig

  }
    ))
p.ci.df = Reduce(function(x, y)
    merge(x, y, by = "row.names", all = T), p.ci)

# dropping correlations
p.ci.df = p.ci.df[!grepl("^cor_", p.ci.df$Row.names),]

#harmonizing the names
p.ci.df$Row.names = gsub("sigma", "Variance of Residual",   p.ci.df$Row.names, perl = T)
p.ci.df$Row.names = gsub("sd_", "Variance of ",   p.ci.df$Row.names, perl = T)
p.ci.df$Row.names = gsub("\\|.*", "",   p.ci.df$Row.names)

if(length(setdiff(random.variances$names, p.ci.df$Row.names))>0)
  stop("Some random effects are missing in the bootstrap results. Possibly due to singular fit. Use fit.stats='random' instead.")


random.p = lapply(random.variances$names, function(x) {
  paste0(
    c("",na_str(f(random.variances[random.variances$names==x,-1],1))),
    na_str(p.ci.df[p.ci.df$Row.names==x,]))

})

return(random.p)

}

na_str <- function(x) {
  x[x=="NA"] <- ""
  x[is.na(x)] <- ""
  x
}


compute_r2 <- function(models) {
  message("Computing R-squared from empty models...")

  # Fit empty models
  empty_models <- lapply(models, function(model) {
    group_var <- names(lme4::getME(model, "flist"))[1]
    formula_str <- paste(". ~ 1 + (1|", group_var, ")")
    stats::update(model, formula_str)
  })

  # Calculate R-squared
  r2_values <- sapply(seq_along(models), function(i) {
    var_empty <- as.data.frame(lme4::VarCorr(empty_models[[i]]))$vcov
    var_full <- as.data.frame(lme4::VarCorr(models[[i]]))$vcov

    # Simplified R-squared calculation
    c(
      "Naive R2 Intercept" = (var_empty[1] - var_full[1]) / var_empty[1],
      "Naive R2 Residual" = (var_empty[length(var_empty)] - var_full[length(var_full)]) /
        var_empty[length(var_empty)]
    )
  })

  r2_values
}

# Helper functions
check_convergence <- function(model) {
  is.null(model@optinfo$conv$lme4$code) &&
    (is.null(model@optinfo$conv$opt) || model@optinfo$conv$opt == 0)
}



generate_stargazer_table <- function(models, extra_lines, model_names,
                                     show_viewer, digits, silent, ...) {



  # Escape underscores for LaTeX
  extra_lines <- lapply(extra_lines, function(line) {
    gsub("_", "\\\\_", line)
  })

  # Generate table

  if (show_viewer && requireNamespace("rstudioapi", quietly = TRUE)) {
    # Create temporary file
    temp_dir <- tempfile()
    dir.create(temp_dir)
    html_file <- file.path(temp_dir, "lmer_table.html")
    # make table
  stargazer_output <- utils::capture.output(
    stargazer::stargazer(
      models,
      out = html_file,
      type = "html",
      summary = FALSE,
      no.space = TRUE,
      single.row = TRUE,
      star.cutoffs = c(0.05, 0.01, 0.001),
      omit.stat = "all",
      column.labels = model_names,
      add.lines = extra_lines,
      digits = digits,
      ...
    )
  )
  # show in viewer
    rstudioapi::viewer(html_file)
  } else {

    stargazer::stargazer(
      models,
      #out = html_file,
      #type = "html",
      summary = FALSE,
      no.space = TRUE,
      #single.row = TRUE,
      #star.cutoffs = c(0.05, 0.01, 0.001),
      omit.stat = "all",
      column.labels = model_names,
      add.lines = extra_lines,
      digits = digits,
      ...
    )

  }


}

#   # random.anova
compute_random_anova <- function(models) {
  # This would need lmerTest implementation
     cat("Computing random.anova: ")
     random.anova  <-  list()
        for( i in 1:length(models)) {
          #cat(i, "of", length(models))
          m <- models[[i]]
           # must assign due to 'ranova' function
            assign(as.character(m@call$data), m@frame)
            ranova.output <- lmerTest::ranova(m)
            rm(list = as.character(m@call$data))

            ranova.output.names = gsub(" in .*$", "",  names(attr(ranova.output, "formulae")))
            ranova.output.names <- paste("LRT of variance of", ranova.output.names)
            ranova.output <- ranova.output[-1,c("LRT", "Df", "Pr(>Chisq)")]
            ranova.output <- apply(ranova.output, 1, function(x)
              paste0(f(x[["LRT"]],1) , "(", x[["Df"]], ")",
                     LittleHelpers:::pvalue_to_stars(x[["Pr(>Chisq)"]])))

           ranova.output <-  data.frame(
            term=ranova.output.names,
            LRT.r.test = ranova.output,
            stringsAsFactors = F
           )
           rownames(ranova.output)<- ranova.output$term

           random.anova[[i]] <- ranova.output

           }


  random.anova <- Reduce(function(a,b) {
    merge(a, b, by="term", all=T,
          suffixes = c(paste0("a", round(rnorm(1,100, 5))), paste0("b", round(rnorm(1,100, 5)))))},
    random.anova
    )
  random.anova <- lapply(1:nrow(random.anova), function(x) gsub("<NA>", NA, random.anova[x,] ))
  random.anova
}


#' Computes pseudo-R2 by subtracting residual variances
#'
#'
#' @param base.model Base model
#' @param tested.model Test model
#'
#' Doesn't make use of Satorra-Bentler correction (should give this option in future).
#' @export
explained_variance <- function (base.model, tested.model) {

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
  d <- Matrix::diag(v)^0.5
  v <- Matrix::diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}




#' Add or remove terms to a lmer formula string
#'
#' @param x Either mer formula object, string containing formula, lmer object. If latter is the case, the updated object will be returned instead of a new formula.
#' @param fixed List of character strings containing variable names or terms to include in the formula as fixed terms.
#' @param random List of character strings containing variable names or terms to include in the formula as random terms.
#' @param drop List of character strings containing variable names or fixed terms to exclude from the formula.
#' @param drop.random List of character strings containing variable names or random terms to exclude from the formula.
#'@param ... Extra arguments passed to `update`. Works only if `x` is a lmer object.
#'
#' @examples f1 <- "a ~ b + c + (1| group)"
#' add_term(f1, , "Conservation")
#' add_term(f1, "Conservation*GDP")
#' add_term(f1, "Conservation*GDP", "Conservation")
#'
#' add_term(f1, drop="female", drop.random = "g.agea")
#'
#' @export

add_term <- function (x, fixed=NULL, random=NULL, drop=NULL, drop.random=NULL, ...) {

require(stringr)
require(stats)
require(lme4)

  if(is(x,"formula")) {
    lmer.formula<-Reduce(paste, deparse((as.formula(x))))
  } else if (is(x,"character")) {
    lmer.formula <- x
  } else if ( any("lmerMod" %in% class(x) )) {
    lmer.formula <-Reduce(paste, deparse(formula(x)))
  }

  f<-paste(
    gsub("\\|", paste(ifelse(!is.null(random),  "+", ""),
                      paste(random, collapse="+"), "|"),
         lmer.formula),
    ifelse(!is.null(fixed),  "+", ""),
    paste(fixed, collapse = " + ")
  )

  if(!is.null(drop)) f<-update(as.formula(f), paste(".~.", paste("-", drop, collapse=" ")))
  if(!is.null(drop.random)) {
    if(is(f,"formula")) f<-Reduce(paste, deparse((as.formula(f))))

    # rf<- grep("\\|", str_extract_all(f, "\\([^()]+\\)")[[1]], value = TRUE)
    # rf.new<- gsub(drop.random, "", grep("\\|", str_extract_all(f, "\\([^()]+\\)")[[1]], value = TRUE))
    # f<-sub(rf, rf.new, f, fixed=T)

    all.terms <- attr(terms.formula(as.formula(f)),"term.labels")
    fixed.terms <- all.terms[-grep("\\|", all.terms)]
    random.terms <- all.terms[grep("\\|", all.terms)]
    group <- str_split(random.terms, "\\|")[[1]][2]
    random.terms <- str_split(random.terms, "\\|")[[1]][1]
    random.terms <- gsub("\\s", "", str_split(random.terms, "\\+")[[1]])
    n.dropped.term <- grep( paste0("^",drop.random, "$"), random.terms)
    if(length(n.dropped.term)!=0) {
      random.terms <- random.terms[-n.dropped.term]
    } else {
      message("drop.random wasn't found in the formula")
    }

    f<-
      paste(as.formula(f)[[2]],
            paste("~", paste(fixed.terms, collapse=" + "),
                  "+ (", paste(random.terms, collapse=" + "),  "|", group, ")"
            ))

  }

  if(is(f,"formula")) f<-Reduce(paste, deparse((as.formula(f))))
  f<-gsub("\\s+", " ",f)
  #verb("nospaces", f)
  f<-gsub("\\+\\s\\|", "\\|", f)


  if(is(x, "formula")| is(x,"character")) {
    return(f)
  }
  if ( any("lmerMod" %in% class(x) )) {
    m <- update(x, f, ...)
    return(m)
  }

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

    re<-sapply(random.terms, function(x) ranef(update(lmer.fit, add_term(lmer.fit@call$formula, random=x))   )[[1]][,x] )

    iv <- aggregate(data[,group.level.terms], list(data[,names(getME(lmer.fit, "flist"))]), mean, na.rm=T)

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


    b <- data.frame(pairs,
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
#'@param variables Character vector of variables names
#'@param group Character of length 1,name of grouping variable
#'@param data Data frame
#'@param prefix Character, added to the befinning of centered variable name.
#'@return Returns an original data frame binded with the new centered variables
#'
#' @export
group_center <- function(variables, group, data, std=FALSE, prefix="g.") {

  new.data <- data[,c(group,variables)]

  if(any(c("tbl", "tibble") %in% class(new.data))) attr(new.data, "class") <- "data.frame"

  for(v in variables)  {

    ag <- tapply(new.data[,v], list(new.data[,group]), mean, na.rm = T)

    for(g in unique(new.data[,group]))
      new.data[new.data[,group]==g, v] <- new.data[new.data[,group]==g, v] -ag[[as.character(g)]]

    if(std) {
      ag.sd<- tapply(new.data[,v], list(new.data[,group]), sd, na.rm = T)
      for(g in unique(new.data[,group])) new.data[new.data[,group]==g, v]<- new.data[new.data[,group]==g, v]/ag.sd[g]
    }
  }
  new.data[,group]<-NULL
  names(new.data)<-paste(prefix, variables, sep="")

  cbind(data, new.data)


}

#' Grand mean centering of one or more variables
#'
#'@param variables Character vector of variables names
#'@param data Data frame
#'@param prefix Character, added to the befinning of centered variable name.
#'@param std Logical. Should the variable be standardized (divided by its std. deviation)?
#' @return Returns an original data frame binded with the new centered variables
#' @export
grand_center <- function(variables, data, prefix="gc.", std = F) {

  new.data <- data[,variables]
  for(v in variables) new.data[,v] = as.vector(scale(new.data[,v], center = T, scale = std))
  names(new.data)<-paste(prefix, variables, sep="")
  cbind(data, new.data)

}

# Creates group level variables####
#' Creates group-level variables by aggregating individual-level variables and adding it to the df
#'
#'
#' @param variables Variable to be aggregated.
#' @param group Grouping variable.
#' @param data A dataset with individual-level data.
#' @param FUN Aggregation function, mean by default.
#' @param prefix Character string, what should be added to the  names of aggregated variables.
#'
#' @returns Returns the original data frame with new group-level variables appended.
#' @details Alternative to `dplyr::group_by` + `dplyr::mutate`. Can be used within functions.
#' @export
aggr_and_merge <- function(variables, group, data, FUN = "mean", prefix = paste0(group, ".")) {

  aggr.dat <- aggregate(data[,variables],
                        setNames(list(data[,group]), nm=group),
                        function(x) eval(call(FUN, x, na.rm=T)))

  colnames(aggr.dat)[-1] <- paste0(prefix, variables)

  appended.data <- merge(data, aggr.dat, by=group, all.x=TRUE)

  return(appended.data)
}



# Corr by group #####
#' Within-group correlations
#'
#' @description Prints correlations in console, and optionally plots in a graph.
#' @param var1 String. Name of variable to correlate.
#' @param var2 String. Name of variable to correlate.
#' @param group Grouping variable.
#' @param data Data.framer containing var1, var2, and group.
#' @param plot Logical. Should the plot be created?
#' @param labs Logical. Should value labels be used?
#' @param highlight.group Characater. What group should be highlighted on graph?
#' @importFrom ggplot2 ggplot
#' @export
cor_within <- function (var1, var2, group, data, plot=TRUE, labs=TRUE, use="pairwise", highlight.group=NA) {

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
                 Corr=t(sapply(unique(Gr),   function(x) {
                   if(all(rowSums(cbind(!is.na(V1[Gr==x]), !is.na(V2[Gr==x])))<2)) {
                     warning(paste("There no valid cases in", x))
                     c(NA, NA, NA)
                   } else {
                   r = cor.test(V1[Gr==x], V2[Gr==x], use=use, conf.level = 0.95)
                   c(r$estimate, r$conf.int)
                   }
                 })),
                 stringsAsFactors = F)
  names(tb)[2:4] <-  c("Corr", "hiCI", "loCI")


  tb$group<-factor(tb$group, levels=tb$group[order(tb$Corr)])
  if(plot==T) {
    tb$group.highlighted <- as.character(tb$group %in% highlight.group)

    g <- ggplot(tb, aes(
      x = group,
      y = Corr,
      ymin = loCI,
      ymax = hiCI
    )) +
      geom_errorbar(width = .4, colour = "grey") +
      geom_point(colour = "red") +
      geom_point(
        aes(colour = group.highlighted, size = group.highlighted),
        show.legend = F,
        alpha = 1
      ) +
      scale_color_manual(values = c("red", "black")) +
      scale_size_manual(values = c(1, 3)) + ylab(paste("Corr", var1, "with", var2)) +
      xlab("") +
      coord_flip() + theme_minimal()
    if (labs)
      g <- g + ggrepel::geom_text_repel(aes(label = (round(Corr, 2))), size =
                                          3)

    print(g)
  }
  return(tb[,-5])

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
  if(any(class(data)=="tbl")) data<-untibble(drop_labs(data))

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
#' Find which random effects could be added to a lmer model
#'@param lmerfit lmer fit
#'@param terms terms, if NA (default) uses each variable from fixed effects
#'@param boot Apply bootstrap?
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
}



#' Extract and export Mplus trace and autocorrelation plots into pdf
#'
#' @param x Either a model read by \code{\link[MplusAutomation]{readModels}}, OR a gh5 file produced by Mplus (in Mplus it should be stated PLOTS: TYPE IS PLOT2;)
#' @param is.file Logical, if x argument is file name. Default is FALSE, (i.e. it's an R object, a model read by \code{\link[MplusAutomation]{readModels}})
#' @param pdffile File name of the pdf report to export all the plots. Defaults to an of x argument with  a '.pdf' extension when is.file == TRUE, and extracts the original filename of the out file if x is a readModels object.
#' @param gg Logical. Whether ggplot  or base R graphics should be used. Default is ggplot which is slower but more flexible, e.g. with many chains.
#' @param param.id Single interger or a vector. Single intterger shows 1:param.id  parameters to plot, a vector shows parameters by their order id. If NULL (default), all the parameters are used.
#' @param niter How many iterations should be shown on a traceplot. NULL (default) refers to all iterations.  However, traceplots for long chains may result in highly overplotted graphics which are slow and take a lot of space on disk. A specified number will take a random sample of values from the iterations.
#' @param raster Logical. Currently not used. It was an option to convert vector plots to raster plots to save space and produce smaller pdfs. Currently not used.
#' @param PSR If PSR (R-hat) should be printed for each parameter. See \code{\link{eachParamPSRMplus}}.
#' @param PSR.version Version of PSR; default is "Rstan" - the most up-to-date, provided by the Rstan::Rhat function (Vehtari et al., 2019). Other possible values: "Gelman" (Gelman et al. (2004)); or "Mplus" (Asparouhov and  Muthen, 2010), and "Naive" which is a raw ratio of sum of a between- and within-chain variances to a within-chain variance.
#' @param ESS Logical. If effective sample size should be printed on the plots. See \code{\link[rstan]{ess_bulk}}
#'
#' @description The function exports all the available plots for each parameter and every chain. Therefore it can take a long time and the resulting pdf file can be large. Each page in pdf is for a specific parameter.
#' @examples
#'
#' \dontrun{
#' m <- MplusAutomation::readModels("mybayes.out")
#' convergencePlotsMplus(m, niter = 5000, PSR.version = "Mplus")
#'
#' # or directly from gh5 file:
#' convergencePlotsMplus("mybayes.gh5", param.id = 1, PSR.version = "Rstan", gg=F)
#' }
#'
#' @export
traceplots_mplus <- function(x,
                                  is.file=F,
                                  pdffile = NULL,
                                  gg=T,
                                  param.id=NULL,
                                  raster=F,
                                  niter=NULL,
                                  chains = NULL,
                                  PSR=TRUE,
                                  PSR.version="Rstan",
                                  ESS = T) {

  require(reshape2)

   if(is.null(pdffile)) pdffile = ifelse(is.file,
                                    paste0(x, ".pdf"),
                                    paste0(x$summaries$Filename,".pdf"))

   print(pdffile)

  if(is.file) {
    print(paste("Reading file ", x ,"..."))
    dat <- rhdf5::h5dump(x)$bayesian_data$parameters_autocorr
  } else {
    dat = x$gh5$bayesian_data$parameters_autocorr
  }


  parnames = dat$statements
  params=dat$parameters
  autocor=dat$autocorrelation

  nparameters = dim(params)[[1]]
  niterations = dim(params)[[2]]
  nchains     = dim(params)[[3]]

  # use all chains if 'chains' is null
  if(is.null(chains)) chains <- 1:nchains

  #clean parnames
  parnames <- gsub("\\s\\s*", " ", parnames)
  parnames <- data.frame(id= 1:length(parnames), parnames)



  #subsetting
  if(!is.null(param.id)) {
    if(length(param.id)>1)
      param.ids = param.id else param.ids = 1:param.id
  } else {
      param.ids = 1:nrow(parnames)
  }

  parnames <-  parnames[param.ids,]


  headers = sapply(parnames$id, function(i) {
    PSR_ESS = eachParamPSRMplus(params, i)
    paste(parnames[parnames$id==i, 2], "\n",
          ifelse(PSR,paste("PSR=",
                           round(PSR_ESS[[PSR.version]],3)),
                 ""),
          ifelse(ESS,paste("ESS_bulk=",
                           round(PSR_ESS[["ESS_bulk"]], 3)),

                 ""),
          ifelse(ESS,paste("ESS_tail=",
                           round(PSR_ESS[["ESS_tail"]], 3)),
                 ""),
          "Est=", paste0(round(median(params[i,  (niterations/2):niterations, ]),3),
                         "(",round(sd(params[i,  (niterations/2):niterations, ]),3), ")"
          )
    )
  })

  names(headers)<-parnames$id


 # dimnames(params)<-list(NULL, 1:niterations, NULL)
 # if(!is.null(niter)) params = params[,sort(sample(1:niterations, niter)),]
  if(!is.null(niter))
    iterations = sort(sample(1:niterations, niter)) else iterations = 1:niterations

  params <- params[param.ids,iterations,chains]
  dimnames(params)<-list(param.ids,NULL,chains)

  a=reshape2::melt(params)
  colnames(a) <- c("parameter", "iteration", "chain", "value")

  autocor <- autocor[,param.ids,chains]
  dimnames(autocor) <- list(NULL,param.ids,chains)
  b = reshape2::melt(autocor)
  colnames(b) <- c("lag", "parameter", "chain", "autocorrelation")

  # Rasterizing plots
  # t.dir = tempdir()
  # t.dir = "temp_png_files"
  # dir.create(t.dir)
  # if(!raster) pdf(file=pdffile) else png(file = paste0(t.dir, "/Rplot%03d.png"), res=72)

  pdf(file=pdffile)
  totaltime=0

  if(gg) {
    require(ggplot2); require(gridExtra, quietly = T)
    cntr=0; pb = txtProgressBar(0, length(parnames$id), style = 3)

    for(i in as.character(parnames$id) ) {
      begin.time = Sys.time()



      gridExtra::grid.arrange(
        # traceplot
        ggplot(a[a$parameter==i #& a$chain %in% chains
                 ,],
               aes(iteration, value, color = as.factor(chain) ))+
          geom_line(alpha=.5)+
          geom_vline(xintercept = length(iterations)/2, col = "black", linetype = "dashed")+
          #scale_color_brewer(palette=2, type = "qual")+
          labs(col="Chain", title=headers[i])+
          theme_minimal(),

       # autocorrelations
        ggplot(b[b$parameter==i  # & b$chain %in% chains
                 ,],
               aes(lag, autocorrelation, fill = as.factor(chain) ))+
          geom_col(alpha=.8)+
          #scale_fill_brewer(palette=2, type = "qual")+
          ylim( ifelse(min(b[b$parameter==i,"autocorrelation"])<0,
                       min(b[b$parameter==i,"autocorrelation"]),
                       0),
                1)+
          geom_hline(yintercept = .1, linetype = "dashed", col = "black")+
          labs(fill="Chain")+theme_minimal()+
          facet_wrap(~chain),
        nrow=2,ncol=1)

      cntr=1+cntr; setTxtProgressBar(pb, cntr)

      timetaken = Sys.time()-begin.time
      totaltime = timetaken + totaltime
      # print(paste0("Plotting ", i, " out of ",length(parnames$id), ". It took ", round(timetaken,1)," ",
      #              attr(timetaken, "units")))

    }
    close(pb)

  } else { # base R graphics option

    cntr=0; pb = txtProgressBar(0, length(parnames$id), style = 3)

    for(i in parnames$id) {
      begin.time = Sys.time()


      layout.vector= c( 1,1, 1:nchains+1)
      if(nchains %% 2) layout.vector = c(layout.vector, 0)
      layout(matrix(layout.vector, ncol=2, byrow=T))
      a.dat = a[a$parameter==i,]

      # trace plot
      plot(a.dat$iteration, a.dat$value,
           type="n", main = headers[i],
           xlab = "iterations", ylab = "parameter")

      for(chain in 1:nchains) {
        lines(a.dat[a.dat$chain==1,"iteration"],
              a.dat[a.dat$chain==chain, "value"],
              col = rainbow(nchains, alpha = .4)[chain])

      }
      abline(v = niterations/2, col = "black", lty = "dashed")

      # autocorr plots
      for(chain in 1:nchains) {
        plot(b[b$parameter==i & b$chain==chain, "lag"],
             b[b$parameter==i & b$chain==chain, "autocorrelation"],
             col = rainbow(nchains, alpha = .4)[chain],
             type = "h", lwd=10, ylim = c(0,1),
             xlab = "lag", ylab = "autocorrelation")
        abline(h=.1, col="black", lty = "dashed")
      }

      cntr=1+cntr; setTxtProgressBar(pb, cntr)

      timetaken = Sys.time()-begin.time
      totaltime = timetaken + totaltime
      # print(paste0("Plotting ", i, " out of ",length(parnames$id),
      #              ". It took ", round(timetaken,1)," ",
      #              attr(timetaken, "units")))

    }
    close(pb)
  }


dev.off()

  message(paste("Total time taken:", round(totaltime,1), attr(totaltime, "units")))



  #disfunctional piece
  # if(raster) {
  #   print("Compiling pdf...")
  #   png.files = paste0(t.dir, "/",list.files(t.dir))
  #
  #   pdf(pdffile)
  #
  #   for(f in png.files) {
  #     grid::grid.raster(png::readPNG(f, native = FALSE), interpolate = F)
  #   }
  #   dev.off()
  #   rm(f)
  # }

  if(file.exists(pdffile)) {
    message(paste0("The file '", pdffile,  "' has been saved to working directory."))
  } else {
    warning("Something went wrong.")
  }

}

# a <- readModels("/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/STAT/political efficacy/participation outputs/OSF/Bayes/B2-random-effects-Bayes.out")

# convergencePlotsMplus(x=a,
#                       param.id = 90:95,
#                       pdffile = "1.pdf",
#                       niter=1000,
#                       chains = 2:4,
#                       ESS=T, gg=T,
#                       is.file=F
#                      )



#' Various versions of PSR
#'
#' @param parameters A 3-dimensional array (typically taken from Mplus-produced gh5 file in gh5$bayesian_data$parameters_autocorr$parameters or from any other), where first dimension is parameters, second dimension is iterations, and third dimension is chains
#' @param id.parameter Integer id of parameter
#' @param iterations.range Range of iterations to use. All available are used by default (NULL).
#'
#' @details Returns a vector of different versions of PSR:
#' \itemize{
#'     \item  "Rstan" the most up-to-date, provided by the  \code{\link[Rstan]{Rhat}} function (Vehtari et al., 2019).
#'     \item  "Gelman" (Gelman et al. (2004)).
#'     \item  "Mplus" (Asparouhov and  Muthen, 2010),
#'     \item  "Naive" which is a raw ratio of sum of a between- and within-chain variances to a within-chain variance.
#'}
#' @examples
#'
#' \dontrun{
#' m <- MplusAutomation::readModels("mybayes.out")
#' eachParamPSRMplus(a$gh5$bayesian_data$parameters_autocorr$parameters, 1)
#' }
#'
#' @export
eachParamPSRMplus <- function(parameters, id.parameter, iterations.range=NULL) {

  m = dim(parameters)[[3]] # nchains
  niterations.total = dim(parameters)[[2]]
  n = niterations.total/2 #burnin removed, niterations
  if(is.null(iterations.range)) iterations.range=n:niterations.total

  # Bahavior in case there's a single chain (fold it and pretend there are two)
  if(m==1) {
    folded.range1 =  iterations.range[1]:(iterations.range[1]+round(length(iterations.range)/2))
    folded.range2 = (iterations.range[1]+round(length(iterations.range)/2)):iterations.range[length(iterations.range)]

    theta_ij = cbind(parameters[id.parameter, folded.range1, 1],
                     parameters[id.parameter, folded.range2, 1])
    m=2

  } else {

    theta_ij = parameters[id.parameter, iterations.range, ]
  }

  # Mplus PSR

  theta.j = colMeans(theta_ij)
  theta.. = mean(theta.j)
  B = sum( (theta.j - theta..)^2)/(m-1)  # or var(theta.j)
  W=sum(colSums(t(apply(theta_ij, 1, function(r) r-theta.j))^2)/n)/m
  Mplus = sqrt((W+B)/W)


  #Gelman et al. 2004:

  psi.j = colMeans(theta_ij)
  psi.. = mean(psi.j)
  B = sum( (psi.j - psi..)^2)* (n/(m-1))  # or var(theta.j)
  sj_squared = colSums(t(apply(theta_ij, 1, function(r) r-psi.j))^2)/(n-1)
  W=sum(sj_squared)/m
  var_psi_given_y = W*(n-1)/n + B/n
  Gelman = sqrt(var_psi_given_y/W)


  # Naive PSR

  between.var = var(apply(theta_ij, 2, mean))
  within.var =  mean(apply(theta_ij, 2, var))
  Naive = sqrt((within.var + between.var)/within.var)

  # Rstan PSR

  Rstan = rstan::Rhat(theta_ij)


  # ESS

  ESS_bulk = rstan::ess_bulk(theta_ij)
  ESS_tail = rstan::ess_tail(theta_ij)

  return(c(Mplus=Mplus, Gelman=Gelman, Rstan = Rstan, Naive=Naive,
           ESS_bulk = ESS_bulk, ESS_tail = ESS_tail))
}

#' Loglikelihood test for MLR estimator - wrapper
#'
#' @description Convenience wrapper for \code{\link[LittleHelpers]{diffTestMLR}}
#' @param reduced  reduced model read by \code{\link[MplusAutomation]{readModels}}
#' @param full   full model read by \code{\link[MplusAutomation]{readModels}}
#' @export

diffTestMLR_ <- function(reduced, full) {
  diffTestMLR(reduced$summaries$LL,
              reduced$summaries$LLCorrectionFactor,
              reduced$summaries$Parameters,
              full$summaries$LL,
              full$summaries$LLCorrectionFactor,
              full$summaries$Parameters
  )
}



#' Loglikelihood test for MLR estimator
#'
#' @description Adapted from http://www.statmodel.com/chidiff.shtml
#' @param L0  loglikelihood values of reduced model
#' @param L1  loglikelihood values of full model
#' @param c0 scaling correction factors of reduced model
#' @param c1 scaling correction factors of full model
#' @param p0 number of parameters, should be p0 < p1
#' @param p1 number of parameters, should be p0 < p1
#' @export
diffTestMLR <- function(L0, c0, p0,
                        L1, c1, p1) {



  # http://www.statmodel.com/chidiff.shtml
  #
  # Difference Testing Using the Loglikelihood
  #
  # Following are the steps needed to compute a chi-square difference test based on loglikelihood values and scaling correction factors obtained with the MLR estimator.
  #
  # Estimate the nested and comparison models using MLR. The printout gives loglikelihood values L0 and L1 for the H0 and H1 models, respectively, as well as scaling correction factors c0 and c1 for the H0 and H1 models, respectively. For example,
  # L0 = -2,606, c0 = 1.450 with 39 parameters (p0 = 39)
  # L1 = -2,583, c1 = 1.546 with 47 parameters (p1 = 47)
  # Compute the difference test scaling correction where p0 is the number of parameters in the nested model and p1 is the number of parameters in the comparison model.
  cd = (p0 * c0 - p1*c1)/(p0 - p1)
  # = (39*1.450 - 47*1.546)/(39 - 47) = 2.014
  # Compute the chi-square difference test (TRd) as follows:
  TRd = -2*(L0 - L1)/cd
  # = -2*(-2606 + 2583)/2.014 = 22.840
  #

  return(list(
    TRd=TRd,
    df = p1 - p0,
    p.value = 1 - pchisq(TRd, p1 - p0, lower.tail = T)
    )
    )

}


#' Summarize parameters from several Mplus models read by 'readModels'
#'
#' @description Creates a table (dataframe) of parameters extacted from Mplus
#' @param models A list of models read by MplusAutomation::readModels
#' @param std If stdyx standardized coefs should be reported.
#' @param se If SEs should be included.
#' @return Returns a single data frame with unstandardized parameters.
#' @export
partable_mplus <- function(models, std=FALSE, se=T) {

  par.list <- lapply(1:length(models),
                     function(i) {
                       x<-models[[i]]

                       if(std) {
                         p <- x$parameters$stdyx.standardized
                       } else {
                         p <- x$parameters$unstandardized
                       }

                       se.lab =
                         ifelse(
                           grepl("bayes", models[[i]]$summaries$Estimator, ignore.case=T),
                            "posterior_sd", "se")


                       p.formatted <-
                         t(apply(p, 1,
                                 function(eachrow)
                                   c(eachrow[1:2],
                                     est=paste0( sprintf("%.2f",
                                                         as.numeric(eachrow["est"])),
                                                 ifelse(se,
                                                        paste0(
                                                          "(", sprintf("%.2f",
                                                                       as.numeric(eachrow[se.lab]
                                                                       )
                                                          ),
                                                          ")"), ""),
                                                 pvalue_to_stars(as.numeric(eachrow["pval"])
                                                 )
                                     )
                                   )))
                       colnames(p.formatted)[3] <- names(models)[[i]]
                       return(p.formatted)
                     })

 out <- Reduce(function(a, b)
    merge(a, b, by = c("paramHeader", "param"), all=T),
    par.list)

return(out)

}




#' Check NDP related to residuals and correlations of models read by 'readModels'
#'
#' @description Prints if any variance is negative oand whether there are correlations above 1 (OUTPUT: stdyx; is required in the Mplus input)
#' @param model A single model read by MplusAutomation::readModels
#' @return Returns a single data frame with unstandardized parameters.
#' @export
checkMplusModel <- function(model) {

  mgml.pars.nonstd <- model$parameters$unstandardized
  mgml.pars.std <- model$parameters$stdyx.standardized

  cat("Are there any negative residual variances?\n")
  if(any(mgml.pars.nonstd[mgml.pars.nonstd$paramHeader=="Residual.Variances","est"]<0)) {
    cat("Yes\n")

    cat("Unstandardized parameters:\n")
    resid.nonstd <- mgml.pars.nonstd[mgml.pars.nonstd$paramHeader=="Residual.Variances",]
    print(resid.nonstd[resid.nonstd$est<0,])

  } else {
    cat("No\n")
  }


  cat("Are there any correlations above 1?\n")


  if(any(mgml.pars.std[grep("\\.WITH", mgml.pars.std$paramHeader),"est"]>1)) {
    cat("Yes\n")

  } else {
    cat("No\n")
  }

  cat("Additional. close to zero standardized residual variances\n")
  resid.std <- mgml.pars.std[mgml.pars.std$paramHeader=="Residual.Variances",]
  resid.std <- resid.std[resid.std$est<.01,]
  print(resid.std[order(resid.std$est),])
  cat("Correlations >0.9:\n")
  mgml.pars.cors <- mgml.pars.std[grep("\\.WITH", mgml.pars.std$paramHeader),]
  print(mgml.pars.cors[mgml.pars.cors$est>.9,])

  cat("Covariances corresponding to correlations >.0.9\n")
  mgml.pars.covs <- mgml.pars.nonstd[grep("\\.WITH", mgml.pars.nonstd$paramHeader),]
  print(mgml.pars.covs[mgml.pars.cors$est>.9,])


}


#' Extracts parameters from Bayesian models produces by Mplus where 'readModels' cannot help
#'
#' @description Extracts parameters from Bayesian models produces by Mplus where 'readModels' cannot help. Currently for latent class models only.
#' @param file Name of the Mplus .out file.
#' @return A data.frame with all parameters.
#' @export
getParamsMplus <- function(file) {
  #file="RWA_BSEM/RWA4_Bayes_alignment30.out"
  str.bdiff <- paste(readLines(file),
                     collapse = "\n")
  mod.reslts <- sub(".*MODEL RESULTS *(.*?) *Categorical Latent Variables.*", "\\1", str.bdiff)
  by.lc = strsplit(mod.reslts, "Latent Class")[[1]]
  zag <- by.lc[[1]]
  zag <- sub("\n\n", "", zag)
  zag <- read.fwf(textConnection(zag), widths = c(20,12,11,11,11,11))[,-1]
  zag <- trimws(gsub("\\s+", " ", paste(zag[1,], zag[2,])))

  by.lc <- by.lc[-1]
  lc.labs <- gsub("^\\s(.*?) *\n\n.*", "\\1", by.lc)

  by.lc.by.par <- strsplit( by.lc, "\n\n")
  by.lc.by.par <- lapply(by.lc.by.par, function(x) x<-x[-1])

  partab.by.lc <- lapply(setNames(by.lc.by.par, nm = lc.labs),
                         function(lc) {

                           parHeaders <- sub("^\\s(.*?) *\n.*", "\\1", lc)
                           a=lapply(lc, function(x)  read.table(text=gsub("\\*\n|\\*$", "\n", x), skip=1) )
                           names(a)<-parHeaders
                           tab.out <- reshape2::melt(a, id.vars = "V1") %>% dcast(L1 + V1 ~ variable)
                           names(tab.out)<-c("parHeaders", "params", zag)
                           tab.out
                         })

  out <- reshape2::melt(partab.by.lc, id.vars = c("parHeaders", "params")) %>%
    reshape2::dcast(L1 + parHeaders + params~ variable)

  out$latent.classes <- sapply(strsplit(out$L1, " \\(|\\)"), `[[`,1)
  out$group.labels   <- sapply(strsplit(out$L1, " \\(|\\)"), `[[`,2)
  out$sig <- pvalue_to_stars(out$`One-Tailed P-Value`)
  out[,-1]
}









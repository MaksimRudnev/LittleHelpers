
#' Extract and export Mplus trace and autocorrelation plots into pdf
#'
#' @param x Either a model read by *MplusAutomation::readModels*, OR a gh5 file produced by Mplus (in Mplus it should be stated PLOTS: TYPE IS PLOT2;)
#' @param is.file Logical, if x argument is file name. Default is FALSE, (i.e. it's an R object, a model read by *MplusAutomation::readModels*)
#' @param pdffile File name of the pdf report to export all the plots. Defaults to value of x argument with  a '.pdf' extension.
#' @param gg Logical. Whether ggplot  or base R graphics should be used. Default is ggplot which is slower.
#' @param param.id Single interger or a vector. Single intterger shows 1:param.id  parameters to plot, a vector shows parameters by their order id. If NULL (default), all the parameters are used.
#' @param niter How many iteractions should be shown on a traceplot. NULL (defaults) is all iterations. However, traceplots for long chains may result in highly overplotted graphics which are slow and take a lot of space on hard drive.
#' @param raster Logical. Currently not used. It was an option to convert vector plots to  raster plot to save space and produce smaller pdfs.
#' @param PSR If PSR (R-hat) should be printed for each parameter.
#' @param PSR.version Version of PSR; default is "Rstan" - the most up-to-date, provided by the Rstan::Rhat function (Vehtari et al., 2019). Other possible values: "Gelman" (Gelman et al. (2004)); or "Mplus" (Asparouhov and  Muthen, 2010), and "Naive" which is a raw ratio of sum of a between- and within-chain variances to a within-chain variance.
#'
#' @description The function exports all the available plots for each parameter and every chain. Therefore it can take a long time and the resulting pdf file can be large. Each page in pdf is for a specific parameter.
#'
#' @export
convergencePlotsMplus <- function(x, is.file=F, pdffile = "", gg=T, param.id=NULL, raster=F, niter=NULL, PSR=TRUE, PSR.version="Rstan") {

  require(reshape2)

   if(pdffile=="") pdffile = ifelse(is.file,
                                    paste0(x, ".pdf"),
                                    paste0(deparse(substitute(x)),".pdf)"))

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

  #clean parnames
  parnames <- gsub("\\s\\s*", " ", parnames)
  parnames <- data.frame(id= 1:length(parnames), parnames)

  #subsetting
  if(!is.null(param.id)) parnames <-  parnames[if(length(param.id)>1) param.id else 1:param.id,]




  headers = sapply(parnames$id, function(i)
    paste(parnames[parnames$id==i, 2], "\n",
          ifelse(PSR,paste("PSR=",
                           round(
                             eachParamPSRMplus(params, i)[[PSR.version]],
                             3)),
                 ""),
          "Est=", paste0(round(median(params[i,  (niterations/2):niterations, ]),3),
                         "(",round(sd(params[i,  (niterations/2):niterations, ]),3), ")"
          )
    )
  )



  dimnames(params)<-list(NULL, 1:niterations, NULL)
  if(!is.null(niter)) params = params[,sort(sample(1:niterations, niter)),]
  a=reshape2::melt(params)
  colnames(a) <- c("parameter", "iteration", "chain", "value")

  b = reshape2::melt(autocor)
  colnames(b) <- c("lag", "parameter", "chain", "autocorrelation")

  # Rasterizing plots
  # t.dir = tempdir()
  # t.dir = "temp_png_files"
  # dir.create(t.dir)
  # if(!raster) pdf(file=pdffile) else png(file = paste0(t.dir, "/Rplot%03d.png"), res=72)

  totaltime=0

  if(gg) {
    require(ggplot2); require(gridExtra, quietly = T)

    for(i in parnames$id) {
      begin.time = Sys.time()


      gridExtra::grid.arrange(
        ggplot(a[a$parameter==i,], aes(iteration, value, color = as.factor(chain) ))+
          geom_line(alpha=.5)+
          geom_vline(xintercept = niterations/2, col = "black", linetype = "dashed")+
          scale_color_brewer(palette=2, type = "qual")+
          labs(col="Chain", title=headers[i])+
          theme_minimal(),

        ggplot(b[b$parameter==i,], aes(lag, autocorrelation, fill = as.factor(chain) ))+
          geom_col(alpha=.8)+
          scale_fill_brewer(palette=2, type = "qual")+
          ylim(0,1)+
          geom_hline(yintercept = .1, linetype = "dashed", col = "black")+
          labs(fill="Chain")+theme_minimal()+
          facet_wrap(~chain),
        nrow=2,ncol=1)

      timetaken = Sys.time()-begin.time
      totaltime = timetaken + totaltime
      print(paste0("Plotting ", i, " out of ",length(parnames$id), ". It took ", round(timetaken,1)," ",
                   attr(timetaken, "units")))

    }

  } else { # base R graphics option

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
      timetaken = Sys.time()-begin.time
      totaltime = timetaken + totaltime
      print(paste0("Plotting ", i, " out of ",length(parnames$id),
                   ". It took ", round(timetaken,1)," ",
                   attr(timetaken, "units")))

    }
  }



  print(paste("Total time taken:", round(totaltime,1), attr(totaltime, "units")))
  dev.off()


  #disfunctional piece
  if(raster) {
    print("Compiling pdf...")
    png.files = paste0(t.dir, "/",list.files(t.dir))

    pdf(pdffile)

    for(f in png.files) {
      grid::grid.raster(png::readPNG(f, native = FALSE), interpolate = F)
    }
    dev.off()
    rm(f)
  }


  if(file.exists(pdffile)) {
    message(paste("The file", pdffile,  "has been saved."))
  } else {
    warning("Something went wrong.")
  }

}

#' Various versions of PSR
#' @param parameters Array typically taken from plus-produced gh5 file in gh5$bayesian_data$parameters_autocorr   or from any other, where first dimension is parameters, second dimension is iterations, and third dimension is chains
#' @param id.parameter Integer id of parameter
#' @param iterations.range Range of iterations to use. All available are used by default (NULL).
#'
#'
#' @export
eachParamPSRMplus <- function(parameters, id.parameter, iterations.range=NULL) {

  m = dim(parameters)[[3]] # nchains
  niterations.total = dim(parameters)[[2]]
  n = niterations.total/2 #burnin removed, niterations
  if(is.null(iterations.range)) iterations.range=n:niterations.total
  if(m==1) {
    folded.range1 =  iterations.range[1]:(iterations.range[1]+round(length(iterations.range)/2))
    folded.range2 = (iterations.range[1]+round(length(iterations.range)/2)):iterations.range[length(iterations.range)]

    theta_ij = cbind(parameters[id.parameter, folded.range1, 1],
                     parameters[id.parameter, folded.range2, 1])
    m=2
  } else {

    theta_ij = parameters[id.parameter, iterations.range, ]
  }
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

  # rstan PSR

  Rstan = rstan::Rhat(theta_ij)

  return(c(Mplus=Mplus, Gelman=Gelman, Naive=Naive, Rstan = Rstan))
}

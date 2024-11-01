#'get PSR from an mplus output file
#'
#'
#'
#' @export
get_psr <- function(path.to.file, last=TRUE, plot = F) {

  if(dir.exists(path.to.file)) {
    files <- paste0(path.to.file, "/", list.files(path.to.file, pattern = "(\\.out)"))
  } else if(file.exists(path.to.file)) {
    files <- path.to.file
  } else {
    stop("File '", path.to.file, "' was not found.")
  }

  sapply(files, function(f) {

    mplus.out <- readLines(f)

    if(any(grepl("(ITERATION    SCALE REDUCTION      HIGHEST PSR)",   mplus.out))) {
      begin <- grep("(ITERATION    SCALE REDUCTION      HIGHEST PSR)",   mplus.out) + 1
      empty <- mplus.out[begin:length(mplus.out)]==""
      end <- na.omit(sapply(1:(length(empty)-2), function(x) ifelse(sum(empty[x:(x+2)])==2, x, NA)) )[1] -1 + begin

      out <- read.table(text = mplus.out[begin:end])
      names(out) <- c("ITERATION", "POTENTIAL SCALE REDUCTION", "PARAMETER WITH HIGHEST PSR")

      if(plot) { plot(x=out[[1]], y=out[[2]], type = "b", xlab="Iteration #", main = "Potential scale reduction", ylab = "", col="violetred")
        abline(h=1,lty="dashed", col= "skyblue4") }

      if(last) return(out[nrow(out),2]) else  return(out)


    } else {

      return(NA)
    }
  })
}


#' Hoot's BRMSEA
#'
#' @description  Adapted from Hoots et al., 2017, https://journals.sagepub.com/doi/suppl/10.1177/0013164417709314
#' @export
brmsea <-
  function(file = "bayesian.gh5",
           file_sel = "Chi-square values",
           ms = TRUE,
           cil = .05,
           ciu = .95,
           allout = FALSE,
           Min1 = TRUE,
           Ngr = 4){


    require(MplusAutomation)
    require(stringr)
    require(rhdf5)


    mplus.get.group.attribute <- function(file, groupstr, attrstr) {
      if ( !(file.exists(file))) {
        cstr <- paste("- file does not exist:",file,"\n")
        stop(cstr)
      }

      gh5 <- h5dump(file, load=TRUE)

      fid <- H5Fopen(file)
      gid <- H5Gopen(fid, groupstr)
      atid <- H5Aopen(gid, attrstr)

      attr <- H5Aread(atid)

      H5Aclose(atid)
      H5Gclose(gid)
      H5Fclose(fid)

      attr <- gsub("(^\\s+|\\s+$)", "", attr, perl=TRUE)

      return(attr)
    }

    # Functions importing gh5s mplus data into R #######
    mplus.get.bayesian.predictive.observed <- function(file,plabel) {
      if (!(file.exists(file))) {
        cstr <- paste("- file does not exist:",file,"\n")
        stop(cstr)
      }

      gh5 <- h5dump(file, load=TRUE)

      # check if bayesian data exists
      if ( !("bayesian_data" %in% names(gh5)) ) {
        stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
      }

      if (missing(plabel)) {
        stop("- requires the predictive label or index.\n\nUse mplus.list.bayesian.predictive.labels to get the list of parameters.")
      }

      if (is.character(plabel)) {
        statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
        statements <- tolower(statements)
        plabel <- tolower(plabel)
        paramidx <- pmatch(plabel, statements, nomatch=0)

        if (paramidx == 0) {
          cstr <- paste(c("- unknown predictive label:"),plabel,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
          stop(cstr)
        }
      } else {
        # get the dimensions of parameters array
        # first dimension is the number of ???
        # second dimension is the number of predictive labels
        dims <- attr(gh5$bayesian_data$predictive$observed,"dim")

        paramidx <- plabel
        if (paramidx < 1 || paramidx > dims[2]) {
          cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
          stop(cstr)
        }
      }

      xx <- gh5$bayesian_data$predictive$observed[,paramidx]
      xx
    }
    mplus.get.bayesian.predictive.replicated <- function(file,plabel) {
      if (!(file.exists(file))) {
        cstr <- paste("- file does not exist:",file,"\n")
        stop(cstr)
      }

      gh5 <- h5dump(file, load=TRUE)

      # check if bayesian data exists
      if ( !("bayesian_data" %in% names(gh5)) ) {
        stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
      }

      if (missing(plabel)) {
        stop("- requires the predictive label or index.\n\nUse mplus.list.bayesian.predictive.labels to get the list of parameters.")
      }

      if (is.character(plabel)) {
        statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
        statements <- tolower(statements)
        plabel <- tolower(plabel)
        paramidx <- pmatch(plabel, statements, nomatch=0)

        if (paramidx == 0) {
          cstr <- paste(c("- unknown predictive label:"),plabel,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
          stop(cstr)
        }
      } else {
        # get the dimensions of parameters array
        # first dimension is the number of ???
        # second dimension is the number of predictive labels
        dims <- attr(gh5$bayesian_data$predictive$replicated,"dim")

        paramidx <- plabel
        if (paramidx < 1 || paramidx > dims[2]) {
          cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
          stop(cstr)
        }
      }

      xx <- gh5$bayesian_data$predictive$replicated[,paramidx]
      xx
    }


    # Read in ######
    obs <- mplus.get.bayesian.predictive.observed(file, file_sel)
    rep <- mplus.get.bayesian.predictive.replicated(file, file_sel)
    # Retrieve pD and N
    #sum <- extractModelSummaries(gsub("gh5", "out", file))
    sum <- readModels(gsub("gh5", "out", file))$summaries
    pD <- sum$pD
    N <- sum$Observations
    nvar = sum$NDependentVars


    # Compute BRMSEA #######

    # # Compute number of parameters
    if(ms) p <- (((nvar * (nvar + 1)) / 2) + nvar)
    if(!ms) p <- (((nvar * (nvar + 1))/ 2) + 0)
    p <- p * Ngr
    # # Substract parameters and estimated parameters
    dif.ppD <- p - pD
    nonc <- ( ( obs-rep ) - dif.ppD )
    # # Correct if numerator is smaller than zero
    nonc[nonc < 0] <- 0
    # # Compute BRMSEA (with or without the -1 correction)
    if(Min1) BRMSEA <- sqrt(nonc / (dif.ppD * (N -1)))*sqrt(Ngr)
    if(!Min1) BRMSEA <- sqrt(nonc / (dif.ppD * N ))*sqrt(Ngr)
    # # Compute posterior probability intervals
    BRMSEA_ci <- quantile(BRMSEA, probs = c(cil, ciu))
    # # Save posterior probability interval or all BRMSEA if(allout) out <- BRMSEA
    if(!allout) out <- BRMSEA_ci
    return(out)
  }

#brmsea("bayesian.gh5", Ngr = 4)


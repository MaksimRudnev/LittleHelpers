#' Hoot's BRMSEA
#'
#' @description  Adapted from Hoots et al., 2017, https://journals.sagepub.com/doi/suppl/10.1177/0013164417709314
#' @param file gh5 file from Mplus
#' @param file_sel Character. The name of the predictive check to be used. Default is "Chi-square values".
#' @param ms Logical. If TRUE, the model is a mean structure model. Default is TRUE.
#' @param cil Lower bound of the posterior probability interval. Default is .05
#' @param ciu Upper bound of the posterior probability interval. Default is .95
#' @param allout Logical. If TRUE, returns all BRMSEA values instead of the posterior probability interval. Default is FALSE.
#' @param Min1 Logical. If TRUE, uses N-1 in the denominator. Default is TRUE.
#' @param Ngr Number of groups in the model. Default is 4.
#'
# brmsea <-
#   function(file = "bayesian.gh5",
#            file_sel = "Chi-square values",
#            ms = TRUE,
#            cil = .05,
#            ciu = .95,
#            allout = FALSE,
#            Min1 = TRUE,
#            Ngr = 4) {
#
#
#     require(MplusAutomation)
#     require(stringr)
#     require(rhdf5)
#
#
#     mplus.get.group.attribute <- function(file, groupstr, attrstr) {
#       if ( !(file.exists(file))) {
#         cstr <- paste("- file does not exist:",file,"\n")
#         stop(cstr)
#       }
#
#       gh5 <- h5dump(file, load=TRUE)
#
#       fid <- H5Fopen(file)
#       gid <- H5Gopen(fid, groupstr)
#       atid <- H5Aopen(gid, attrstr)
#
#       attr <- H5Aread(atid)
#
#       H5Aclose(atid)
#       H5Gclose(gid)
#       H5Fclose(fid)
#
#       attr <- gsub("(^\\s+|\\s+$)", "", attr, perl=TRUE)
#
#       return(attr)
#     }
#
#     ## Functions importing gh5s mplus data into R #######
#     mplus.get.bayesian.predictive.observed <- function(file,plabel) {
#       if (!(file.exists(file))) {
#         cstr <- paste("- file does not exist:",file,"\n")
#         stop(cstr)
#       }
#
#       gh5 <- h5dump(file, load=TRUE)
#
#       # check if bayesian data exists
#       if ( !("bayesian_data" %in% names(gh5)) ) {
#         stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
#       }
#
#       if (missing(plabel)) {
#         stop("- requires the predictive label or index.\n\nUse mplus.list.bayesian.predictive.labels to get the list of parameters.")
#       }
#
#       if (is.character(plabel)) {
#         statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
#         statements <- tolower(statements)
#         plabel <- tolower(plabel)
#         paramidx <- pmatch(plabel, statements, nomatch=0)
#
#         if (paramidx == 0) {
#           cstr <- paste(c("- unknown predictive label:"),plabel,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
#           stop(cstr)
#         }
#       } else {
#         # get the dimensions of parameters array
#         # first dimension is the number of ???
#         # second dimension is the number of predictive labels
#         dims <- attr(gh5$bayesian_data$predictive$observed,"dim")
#
#         paramidx <- plabel
#         if (paramidx < 1 || paramidx > dims[2]) {
#           cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
#           stop(cstr)
#         }
#       }
#
#       xx <- gh5$bayesian_data$predictive$observed[,paramidx]
#       xx
#     }
#     mplus.get.bayesian.predictive.replicated <- function(file,plabel) {
#       if (!(file.exists(file))) {
#         cstr <- paste("- file does not exist:",file,"\n")
#         stop(cstr)
#       }
#
#       gh5 <- h5dump(file, load=TRUE)
#
#       # check if bayesian data exists
#       if ( !("bayesian_data" %in% names(gh5)) ) {
#         stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
#       }
#
#       if (missing(plabel)) {
#         stop("- requires the predictive label or index.\n\nUse mplus.list.bayesian.predictive.labels to get the list of parameters.")
#       }
#
#       if (is.character(plabel)) {
#         statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
#         statements <- tolower(statements)
#         plabel <- tolower(plabel)
#         paramidx <- pmatch(plabel, statements, nomatch=0)
#
#         if (paramidx == 0) {
#           cstr <- paste(c("- unknown predictive label:"),plabel,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
#           stop(cstr)
#         }
#       } else {
#         # get the dimensions of parameters array
#         # first dimension is the number of ???
#         # second dimension is the number of predictive labels
#         dims <- attr(gh5$bayesian_data$predictive$replicated,"dim")
#
#         paramidx <- plabel
#         if (paramidx < 1 || paramidx > dims[2]) {
#           cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.predictive.labels to see the list of parameters.\n")
#           stop(cstr)
#         }
#       }
#
#       xx <- gh5$bayesian_data$predictive$replicated[,paramidx]
#       xx
#     }
#
#
#     ## Read in ######
#     obs <- mplus.get.bayesian.predictive.observed(file, file_sel)
#     rep <- mplus.get.bayesian.predictive.replicated(file, file_sel)
#     # Retrieve pD and N
#     #sum <- extractModelSummaries(gsub("gh5", "out", file))
#     sum <- readModels(gsub("gh5", "out", file))$summaries
#     pD <- sum$pD
#     N <- sum$Observations
#     nvar = sum$NDependentVars
#
#
#     ## Compute BRMSEA #######
#
#     # # Compute number of parameters
#     if(ms) p <- (((nvar * (nvar + 1)) / 2) + nvar)
#     if(!ms) p <- (((nvar * (nvar + 1))/ 2) + 0)
#     p <- p * Ngr
#     # # Substract parameters and estimated parameters
#     dif.ppD <- p - pD
#     nonc <- ( ( obs-rep ) - dif.ppD )
#     # # Correct if numerator is smaller than zero
#     nonc[nonc < 0] <- 0
#     # # Compute BRMSEA (with or without the -1 correction)
#     if(Min1) BRMSEA <- sqrt(nonc / (dif.ppD * (N -1)))*sqrt(Ngr)
#     if(!Min1) BRMSEA <- sqrt(nonc / (dif.ppD * N ))*sqrt(Ngr)
#     # # Compute posterior probability intervals
#     BRMSEA_ci <- quantile(BRMSEA, probs = c(cil, ciu))
#     # # Save posterior probability interval or all BRMSEA if(allout) out <- BRMSEA
#     if(!allout) out <- BRMSEA_ci
#     return(out)
#   }


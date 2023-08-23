#' Computes Gamma fit index for SEM models fit in Mplus
#'
#' @param mplus.model Mplus model read by \link[MplusAutomation]{readModels} function.
#' @details Adapted from \link[semTools]{moreFitIndices}
#'
#' @export

gammaHat.mplus = function(mplus.model) {

  fit = mplus.model$summaries
  p = fit[["NDependentVars"]]
  n = fit[["Observations"]]
  ngroup = fit[["NGroups"]]
  # formulas adapted from semTools
  gammaHat <- p/(p + 2 * ((fit[["ChiSqM_Value"]] - fit[["ChiSqM_DF"]])/n))

  adjGammaHat <- 1 - (((ngroup * p * (p + 1))/2)/fit["ChiSqM_DF"]) * (1 - gammaHat)

  list(gammaHat = gammaHat, adjGammaHat = adjGammaHat)

}

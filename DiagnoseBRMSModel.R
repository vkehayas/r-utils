DiagnoseBRMSModel = function(mdl,
                             mdlName = deparse(substitute(mdl)),
                             verbose = 2) {

  CheckDivergence = function(mdl, mdlName, verbose) {

    if (brms::is.brmsfit(mdl)) {

      divVector = rstan::get_divergent_iterations(mdl$fit)
      n = length(divVector)
      nDiv = sum(divVector)

      dispString = sprintf("There were %s divergent transitions out of %s iterations for model %s",
                           nDiv, n, mdlName)

      if (nDiv > 0) {

        hasDivergent = TRUE

        if (verbose == 2) {

          warning(dispString)

        }
      } else {

        hasDivergent = FALSE

        if (verbose == 2) {

          message(dispString)

        }
      }

    } else {

      stop(sprintf("%s is not a brmsfit object",
                   mdlName))

    }

    return(hasDivergent)

  }

  CheckTreeDepth = function(mdl, mdlName, verbose) {

    if (brms::is.brmsfitis.brmsfit(mdl)) {

      treeDepthVector = rstan::get_max_treedepth_iterations(mdl$fit)
      n = length(treeDepthVector)
      nMaxTreeDepth = sum(treeDepthVector)

      dispString = sprintf("%s out of %s iterations saturated the maximum tree depth for model %s",
                           nMaxTreeDepth, n, mdlName)

      if (nMaxTreeDepth > 0) {

        hasMaxTreeDepth = TRUE

        if (verbose == 2) {

          warning(dispString)

        }

      } else {

        hasMaxTreeDepth = FALSE

        if (verbose == 2) {

          message(dispString)

        }
      }

    } else {

      stop(sprintf("%s is not a brmsfit object",
                   mdlName))

    }

    return(hasMaxTreeDepth)

  }

  CheckEnergy = function(mdl, mdlName, verbose) {

    if (brms::is.brmsfit(mdl)) {

      nLowBFMI = rstan::get_low_bfmi_chains(mdl$fit)
      nChains = length(rstan::get_bfmi(mdl$fit))

      if (length(nLowBFMI) != 0L) {

        hasLowBFMI = TRUE
        dispString = sprintf("%s out of %s chains had a low E-BFMI value for model %s",
                             nLowBFMI, nChains, mdlName)

        if (verbose == 2) {

          warning(dispString)

        }

      } else {

        hasLowBFMI = FALSE
        nLowBFMI = 0L
        dispString = sprintf("%s out of %s chains had a low E-BFMI value for model %s",
                             nLowBFMI, nChains, mdlName)

        if (verbose == 2) {

          message(dispString)

        }

      }

    } else {

      stop(sprintf("%s is not a brmsfit object",
                   mdlName))

    }

    return(hasLowBFMI)

  }

  CheckNEffective = function(mdl, mdlName, verbose) {

    mdlSummary = rstan::summary(mdl$fit, prob = 0.5)$summary
    restrictedModelSummary = mdlSummary[!grepl("^L", rownames(mdlSummary)), ]
    nEff = restrictedModelSummary[, ncol(restrictedModelSummary) - 1]
    nIter = mdl$fit@sim$iter
    pcEff = nEff/nIter
    nLowNEff = sum(pcEff < 0.1)
    dispString = sprintf("%s parameters had nEff/nIter below 0.1 for model %s",
                         nLowNEff, mdlName)

    if (brms::is.brmsfit(mdl)) {

      if (nLowNEff != 0) {

        hasSmallNEff = TRUE

        if (verbose == 2) {

          warning(dispString)

        }

      } else {

        hasSmallNEff = FALSE

        if (verbose == 2) {

          message(dispString)

        }

      }

    } else {

      stop(sprintf("%s is not a brmsfit object",
                   mdlName))

    }

    return(hasSmallNEff)

  }

  CheckRHat = function(mdl, mdlName, verbose) {

    mdlSummary = rstan::summary(mdl$fit, prob = 0.5)$summary
    restrictedModelSummary = mdlSummary[!grepl("^L", rownames(mdlSummary)), ]
    rHats = restrictedModelSummary[, ncol(restrictedModelSummary)]
    nRHatsLarge = sum(rHats > 1.1)
    dispString = sprintf("%s R-hat values were > 1.1 for model %s",
                         nRHatsLarge, mdlName)

    if (brms::is.brmsfit(mdl)) {

      if (nRHatsLarge != 0) {

        hasLargeRHats = TRUE

        if (verbose == 2) {

          warning(dispString)

        }

      } else {

        hasLargeRHats = FALSE

        if (verbose == 2) {

          message(dispString)

        }

      }

    } else {

      stop(sprintf("%s is not a brmsfit object",
                   mdlName))

    }

    return(hasLargeRHats)

  }


  modelDiagnostics = list(hasDivergent = CheckDivergence(mdl, mdlName, verbose),
                          hasMaxTreeDepth = CheckTreeDepth(mdl, mdlName, verbose),
                          hasLowBFMI = CheckEnergy(mdl, mdlName, verbose),
                          hasLargeRHats = CheckRHat(mdl, mdlName, verbose),
                          hasSmallNEff = CheckNEffective(mdl, mdlName, verbose))

  allClear = unlist(lapply(modelDiagnostics, isFALSE))

  modelDiagnostics$allClear = all(allClear)
  modelDiagnostics = as.data.frame(modelDiagnostics)
  modelDiagnostics$modelName = mdlName


  if (verbose >= 1) {

    if (all(allClear)) {

      message(sprintf("No problems found for model %s",
                      mdlName))

    } else {

      warning(sprintf("Some diagnostics failed for model %s",
                      mdlName))

    }

  }

  return(modelDiagnostics)

}

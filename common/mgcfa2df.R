mgcfa2df <- function(mod, cnames = c('chisq','df','cfi','tli','cfi.robust','tli.robust'
                                     ,'rmsea','rmsea.ci.lower','rmsea.ci.upper'
                                     ,'rmsea.robust','rmsea.ci.lower.robust','rmsea.ci.upper.robust')) {
  df <- do.call(rbind, lapply(mod$fit.mdls, FUN = function(fit_mdl) {
    fit_df <- tryCatch(
      t(as.data.frame(fitmeasures(fit_mdl$cfa, cnames)))
      , error = function(e) { t(as.data.frame(sapply(cnames, FUN = function(x) NA))) })
    aov_df <- data.frame(AIC=NA, BIC=NA, 'Chisq diff'=NA, 'Df diff'=NA, 'Pr(>Chisq)'=NA)
    colnames(aov_df) <- c('AIC','BIC','Chisq diff','Df diff','Pr(>Chisq)')
    if ("aov" %in% names(fit_mdl) &&  typeof(fit_mdl$aov) != 'character') {
      aov_df <- as.data.frame(fit_mdl$aov[2,c('AIC','BIC','Chisq diff','Df diff','Pr(>Chisq)')])
    }
    
    obs <- NA
    if (!is.na(aov_df[["Pr(>Chisq)"]]) && aov_df[["Pr(>Chisq)"]] < 0.05) obs <- "sig. diff"
    return(cbind(group= mod$group, value=mod$value, mdl=fit_mdl$name, n=fit_mdl$n, fit_df, aov_df, obs=obs))
  }))
  return(df)
}

#mgcfa_df$obs <- c(NA, sapply(seq(2, nrow(mgcfa_df)), FUN = function(i) {
#  to_return <- mgcfa_df$obs[i]
#  if (mgcfa_df$mdl[i] %in% c('Metric model','Scalar model','Strict model','Factor model')) {
#    if (is.na(mgcfa_df$cfi[i])) return("cfi is undefined")
#    if (is.na(mgcfa_df$cfi[i-1])) return("previous cfi is undefined")
#    if (is.na(mgcfa_df$`Pr(>Chisq)`[i]) && abs(mgcfa_df$cfi[i] - mgcfa_df$cfi[i-1]) > 0.01) {
#      to_return <- "diff cfi"
#    }
#  }
#  return(to_return)
#}))

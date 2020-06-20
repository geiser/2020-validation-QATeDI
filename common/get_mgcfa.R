get_mgcfa <- function(mdl, dat, group_by= "group", value = "value", std.lv = F, fix.config = list()) {
  
  sdat <- mutate(select(dat, starts_with(group_by), starts_with("Item"))
                 , group = if_else(dat[[group_by]] == value, 1, 2))
  sdat[["group"]] <- factor(sdat$group, levels = c(1,2), labels = c(as.character(value), paste("Not is", as.character(value))))
  
  mdl_configs <- list()
  mdl_configs[["overall.fit"]] <- list(name='Overall model', data = sdat)
  mdl_configs[["fit.configural.in"]] <- list(name=paste(as.character(value),'model')
                                             , data = sdat[which(sdat$group == as.character(value)),])
  mdl_configs[["fit.configural.not"]] <- list(name=paste('Not in',as.character(value),'model')
                                              , data = sdat[which(sdat$group != as.character(value)),])
  mdl_configs[["fit.combine.groups"]] <- list(name='Configural model', data = sdat, group = "group")
  
  mdl_configs[["fit.metric"]] <- list(name='Metric model', data = sdat, group = "group", group.equal = c("loadings"))
  if (!is.null(fix.config[["fit.metric.alt"]])) {
    mdl_configs[["fit.metric.alt"]] <- list(name='Metric model', data = sdat, group = "group", group.equal = c("loadings"))
    mdl_configs[["fit.metric.alt"]][["name"]] <- fix.config[["fit.metric.alt"]]$name
    mdl_configs[["fit.metric.alt"]][["group.partial"]] <- fix.config[["fit.metric.alt"]]$group.partial
  }
  
  mdl_configs[["fit.scalar"]] <- list(name='Scalar model', data = sdat, group = "group", group.equal = c("loadings","intercepts"))
  if (!is.null(fix.config[["fit.scalar.alt"]])) {
    mdl_configs[["fit.scalar.alt"]] <- list(name='Scalar model', data = sdat, group = "group", group.equal = c("loadings","intercepts"))
    mdl_configs[["fit.scalar.alt"]][["name"]] <- fix.config[["fit.scalar.alt"]]$name
    mdl_configs[["fit.scalar.alt"]][["group.partial"]] <- fix.config[["fit.scalar.alt"]]$group.partial
  }
  
  mdl_configs[["fit.strict.residuals"]] <- list(name='Strict model', data = sdat, group = "group", group.equal = c("loadings","intercepts","residuals"))
  if (!is.null(fix.config[["fit.strict.residuals.alt"]])) {
    mdl_configs[["fit.strict.residuals.alt"]] <- list(name='Strict model', data = sdat, group = "group", group.equal = c("loadings","intercepts","residuals"))
    mdl_configs[["fit.strict.residuals.alt"]][["name"]] <- fix.config[["fit.strict.residuals.alt"]]$name
    mdl_configs[["fit.strict.residuals.alt"]][["group.partial"]] <- fix.config[["fit.strict.residuals.alt"]]$group.partial
  }
  
  mdl_configs[["fit.varfactor"]] <- list(name='Factor model', data = sdat, group = "group", group.equal = c("loadings","intercepts","residuals","lv.variances","lv.covariances"))
  if (!is.null(fix.config[["fit.varfactor.alt"]])) {
    mdl_configs[["fit.varfactor.alt"]] <- list(name='Factor model', data = sdat, group = "group", group.equal = c("loadings","intercepts","residuals","lv.variances","lv.covariances"))
    mdl_configs[["fit.varfactor.alt"]][["name"]] <- fix.config[["fit.varfactor.alt"]]$name
    mdl_configs[["fit.varfactor.alt"]][["group.partial"]] <- fix.config[["fit.varfactor.alt"]]$group.partial
  }
  
  fit.mdls <- lapply(mdl_configs, FUN = function(x) {
    parameters <- c(model = mdl, x, std.lv = std.lv, estimator = "MLR", meanstructure = T)
    parameters[['name']] <- NULL
    return(list(name=x$name, n=nrow(x$data), cfa=do.call("cfa", parameters), data=x$data))
  })
  
  fit.mdls$fit.metric[["aov"]] <- tryCatch(anova(fit.mdls$fit.metric$cfa,  fit.mdls$fit.combine.groups$cfa), error = function(e) e$message)
  if (!is.null(fix.config[["fit.metric.alt"]])) {
    fit.mdls$fit.metric.alt[["aov"]] <- tryCatch(anova(fit.mdls$fit.metric.alt$cfa, fit.mdls[[fix.config$fit.metric.alt$aov]]$cfa), error = function(e) e$message)
  }
  
  fit.mdls$fit.scalar[["aov"]] <- tryCatch(anova(fit.mdls$fit.scalar$cfa,  fit.mdls$fit.metric$cfa), error = function(e) e$message)
  if (!is.null(fix.config[["fit.scalar.alt"]])) {
    fit.mdls$fit.scalar.alt[["aov"]] <- tryCatch(anova(fit.mdls$fit.scalar.alt$cfa, fit.mdls[[fix.config$fit.scalar.alt$aov]]$cfa), error = function(e) e$message)
  }
  
  fit.mdls$fit.strict.residuals[["aov"]] <- tryCatch(anova(fit.mdls$fit.strict.residuals$cfa, fit.mdls$fit.scalar$cfa), error = function(e) e$message)
  if (!is.null(fix.config[["fit.strict.residuals.alt"]])) {
    fit.mdls$fit.strict.residuals.alt[["aov"]] <- tryCatch(anova(fit.mdls$fit.strict.residuals.alt$cfa, fit.mdls[[fix.config$fit.strict.residuals.alt$aov]]$cfa), error = function(e) e$message)
  }
  
  fit.mdls$fit.varfactor[["aov"]] <- tryCatch(anova(fit.mdls$fit.varfactor$cfa, fit.mdls$fit.strict.residuals$cfa), error = function(e) e$message)
  if (!is.null(fix.config[["fit.varfactor.alt"]])) {
    fit.mdls$fit.varfactor.alt[["aov"]] <- tryCatch(anova(fit.mdls$fit.varfactor.alt$cfa, fit.mdls[[fix.config$fit.varfactor.alt$aov]]$cfa), error = function(e) e$message)
  }
  
  return(list(group = group_by, value = as.character(value), fit.mdls = fit.mdls))
}

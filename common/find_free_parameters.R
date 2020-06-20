
find_free_parameters <- function(mdl, fit_mod, to_fit_mdl, group = "group", group.partial = c()
                                 , std.lv = F, include.combn = F, limit.combn = 0) {
  configs <- list(
    "Metric model" = list(group.partial = c("ML4 =~ Item8", "ML4 =~ Item9", "ML4 =~ Item10"
                                                , "ML1 =~ Item11", "ML1 =~ Item12", "ML1 =~ Item13"
                                                , "ML5 =~ Item14", "ML5 =~ Item15", "ML5 =~ Item16"
                                                , "ML2 =~ Item17", "ML2 =~ Item18", "ML2 =~ Item19"
                                                , "ML6 =~ Item20", "ML6 =~ Item21"
                                                , "ML3 =~ Item22", "ML3 =~ Item23"
                                                , "INF =~ ML4", "INF =~ ML1", "INF =~ ML5", "INF =~ ML2"
                                                , "EXP =~ ML3", "EXP =~ ML6")
                              , group.equal = c("loadings")) 
    , "Scalar model" = list(group.partial = paste(colnames(select(fit_mod$data, starts_with("Item"))), '~ 1')
                            , group.equal = c("loadings","intercepts"))
    , "Strict model" = list(group.partial = c(paste(colnames(select(fit_mod$data, starts_with("Item")))
                                                    , '~~', colnames(select(fit_mod$data, starts_with("Item"))))
                                              ,'ML1 ~~ ML1','ML2 ~~ ML2','ML3 ~~ ML3','ML4 ~~ ML4','ML5 ~~ ML5'
                                              ,'INF ~~ INF', 'EXP ~~ EXP')
                            , group.equal = c("loadings","intercepts","residuals"))
    , "Factor model" = list(group.partial = c('INF ~~ EXP', 'ML1 ~~ ML1','ML2 ~~ ML2','ML3 ~~ ML3','ML4 ~~ ML4','ML5 ~~ ML5'
                                              ,'INF ~~ INF', 'EXP ~~ EXP')
                            , group.equal = c("loadings","intercepts","residuals","lv.variances","lv.covariances"))
  )
  
  to_return <- data.frame()
  partial_syntaxes <- configs[[to_fit_mdl]][["group.partial"]] 
  if (limit.combn == 0) limit.combn <- length(partial_syntaxes)
  
  ##
  tmp_mod <- tryCatch(cfa(model = mdl, data = fit_mod$data, group = group
                          , group.equal = configs[[to_fit_mdl]][["group.equal"]]
                          , std.lv = std.lv, estimator = "MLR", meanstructure = T
                          , group.partial = group.partial)
                      , error = function(e) NULL)
  aov <- tryCatch(anova(tmp_mod, fit_mod$cfa), error = function(e) NULL)
  if (!is.null(tmp_mod) && !is.null(aov)) {
    obs <- ifelse(any(aov$`Pr(>Chisq)` >= 0.05, na.rm=T), "good fit", NA)
    cfi <- fitmeasures(tmp_mod, "cfi")
    diff_cfi <- as.double(fitmeasures(tmp_mod, "cfi") - fitmeasures(fit_mod$cfa, "cfi"))
    to_return <- rbind(data.frame("combn" = 0, "n" = -1, "group.part" = ''
                                  , "group.partial" = paste0(group.partial, collapse = ' '), "cfi" = cfi
                                  , "diff cfi" =  diff_cfi, "Pr.chisq" = aov$`Pr(>Chisq)`[2], "obs" = obs), to_return) 
  }
  if (any(!is.na(to_return$obs))) return(to_return)
  
  ##
  in.group.partial <- c()
  repeat{
    max.diff.cfi <- -Inf
    max.group.partial <- c()
    for (partial in setdiff(partial_syntaxes, in.group.partial)) {
      group.partial.alt <- sort(unique(c(group.partial, in.group.partial, partial)))
      print(paste0(c("exploring group.partial as ... ", group.partial.alt), collapse = ' '))
      
      tmp_mod <- tryCatch(cfa(model = mdl, data = fit_mod$data, group = group
                              , group.equal = configs[[to_fit_mdl]][["group.equal"]]
                              , std.lv = std.lv, estimator = "MLR", meanstructure = T
                              , group.partial = group.partial.alt)
                          , error = function(e) NULL)
      if (is.null(tmp_mod)) next();
      
      aov <- tryCatch(anova(tmp_mod, fit_mod$cfa), error = function(e) NULL)
      if (is.null(aov)) next();
      
      obs <- ifelse(any(aov$`Pr(>Chisq)` >= 0.05, na.rm=T), "good fit", NA)
      cfi <- fitmeasures(tmp_mod, "cfi")
      diff_cfi <- as.double(fitmeasures(tmp_mod, "cfi") - fitmeasures(fit_mod$cfa, "cfi"))
      if (max.diff.cfi < diff_cfi) {
        max.diff.cfi <-  diff_cfi
        max.group.partial <- partial
      }
      to_return <- rbind(data.frame("combn" = 0, "n" = length(group.partial.alt)
                                    , "group.part" = paste0(setdiff(group.partial.alt, group.partial), collapse = ' ')
                                    , "group.partial" =  paste0(group.partial.alt, collapse = ' '), "cfi" = cfi
                                    , "diff cfi" =  diff_cfi, "Pr.chisq" = aov$`Pr(>Chisq)`[2], "obs" = obs), to_return)
    }
    
    if (any(!is.na(to_return$obs))) break();
    in.group.partial <- c(max.group.partial, in.group.partial)
    if (length(partial_syntaxes) == length(in.group.partial)) break();
  }
  
  ##
  if (include.combn) {
    for (i in seq(1, limit.combn)) {
      combination_syntaxes <- combn(partial_syntaxes, i)
      for (j  in seq(1,ncol(combination_syntaxes))) {
        print(paste("Exploring",j,"element from combn(",limit.combn,",",i,")"))
        
        group.partial.alt <- sort(c(group.partial, combination_syntaxes[,j]))
        if (paste0(group.partial.alt, collapse = ' ') %in% to_return[["group.partial"]]) next()
        
        tmp_mod <- tryCatch(cfa(model = mdl, data = fit_mod$data, group = group
                                , group.equal = configs[[to_fit_mdl]][["group.equal"]]
                                , std.lv = std.lv, estimator = "MLR", meanstructure = T
                                , group.partial = group.partial.alt), error = function(e) NULL)
        if (is.null(tmp_mod)) next();
        
        aov <- tryCatch(anova(tmp_mod, fit_mod$cfa), error = function(e) NULL)
        if (is.null(aov)) next();
        
        obs <- ifelse(any(aov$`Pr(>Chisq)` >= 0.05, na.rm=T), "good fit", NA)
        to_return <- rbind(to_return, data.frame(
          "combn" = i, "n" = length(group.partial.alt)
          , "group.part" = paste0(setdiff(group.partial.alt, group.partial), collapse = ' ')
          , "group.partial" = paste0(group.partial.alt, collapse = ' ')
          , "cfi" = as.double(fitmeasures(tmp_mod, "cfi"))
          , "diff cfi" =  as.double(fitmeasures(tmp_mod, "cfi") - fitmeasures(fit_mod$cfa, "cfi"))
          , "Pr.chisq" = aov$`Pr(>Chisq)`[2], "obs" = obs))
      }
      
      if (any(!is.na(to_return$obs))) return(to_return)
    }
  }
  
  return(to_return)
}

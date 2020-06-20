
generate_all_mgcfa <- function(mdl, dat, groups, skips = list(), fix.configs= list()) {
  lgroups <- as.list(groups)
  names(lgroups) <- groups
  
  mgcfa_mods <- lapply(lgroups, FUN = function(lgroup) {
    nvalues <- as.character(unique(dat[[lgroup]]))
    if (lgroup %in% names(skips)) {
      nvalues <- nvalues[!nvalues %in% skips[[lgroup]]]
    }
    lvalues <- as.list(nvalues)
    names(lvalues) <- as.character(nvalues)
    
    lapply(lvalues, FUN = function(lvalue) {
      fix.config <- fix.configs[[paste0(lgroup,':',lvalue)]]
      print(paste0(c(lgroup, lvalue), collapse = ':'))
      
      return(get_mgcfa(mdl, dat, group_by = lgroup, value = lvalue, fix.config = fix.config))
    })
  })
  
  mgcfa_df <- do.call(rbind, lapply(mgcfa_mods, FUN = function(mgcfa_mod) {
    do.call(rbind, lapply(mgcfa_mod, FUN = function(mod) {
      return(mgcfa2df(mod))
    }))
  }))
  
  return(list(mods = mgcfa_mods, df=mgcfa_df))
}

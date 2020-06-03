wants <- c('readr','dplyr','psych','lavaan','ggraph','semPlot','robustHD','GPArotation')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options(stringsAsFactors = FALSE)

library(readr)
library(dplyr)
library(psych)
library(lavaan)
library(ggraph)
library(semPlot)

library(xlsx)
library(r2excel)
source('common/excel/write_alpha_in_workbook.R')

library(MVN)
library(daff)
library(robustHD)


fdat <- read_csv('data/data-fa.csv')
ugroups <- read_csv('data/user-groups.csv')
dat <- merge(ugroups, fdat)


groups <- unique(expand.grid(Unidade=c(NA,unique(dat$Unidade))
                             , Nivel=c(NA,unique(dat$Nivel))
                             , Modalidade=c(NA,unique(dat$Modalidade))
                             , stringsAsFactors = F))
reliability_df <- do.call(rbind, lapply(seq(1,nrow(groups)), FUN = function(i) {
  sdat <- dat; group <- groups[i,]
  if (!is.na(group$Unidade)) sdat <- sdat[sdat$Unidade == group$Unidade,]
  if (!is.na(group$Nivel)) sdat <- sdat[sdat$Nivel == group$Nivel,]
  if (!is.na(group$Modalidade)) sdat <- sdat[sdat$Modalidade == group$Modalidade,]
  sdat <- select(sdat, starts_with('Item'))
  
  if (nrow(sdat) > 30) {
    alpha_mods <- list(
      'all'=list(factor='all', mod=psych::alpha(sdat))
      , 'WLS1'=list(factor='WLS1', mod=psych::alpha(sdat[,c('Item9','Item10','Item12','Item13')]))
      , 'WLS2'=list(factor='WLS2', mod=psych::alpha(sdat[,c('Item18','Item19')]))
      , 'WLS3'=list(factor='WLS3', mod=psych::alpha(sdat[,c('Item22','Item23')]))
      , 'WLS4'=list(factor='WLS4', mod=psych::alpha(sdat[,c('Item15','Item16')]))
      , 'WLS5'=list(factor='WLS5', mod=psych::alpha(sdat[,c('Item20','Item21')]))
      , 'WLS1+WLS3+WLS4+WLS5'=list(factor='WLS1+WLS3+WLS4+WLS5', mod=psych::alpha(sdat[,c('Item9','Item10','Item12','Item13','Item22','Item23','Item15','Item16','Item20','Item21')]))
    )
    
    # write reliability analysis
    filename <- "report/reliability/"
    if (is.na(group$Unidade) & is.na(group$Nivel) & is.na(group$Modalidade)) {
      filename <- paste0(filename, "full-all.xlsx")
    } else if (!is.na(group$Unidade) & is.na(group$Nivel) & is.na(group$Modalidade)) {
      filename <- paste0(filename, "by-unidade/", group$Unidade,".xlsx")
    } else if (is.na(group$Unidade) & !is.na(group$Nivel) & is.na(group$Modalidade)) {
      filename <- paste0(filename, "by-nivel/", group$Nivel,".xlsx")
    } else if (is.na(group$Unidade) & is.na(group$Nivel) & !is.na(group$Modalidade)) {
      filename <- paste0(filename, "by-modalidade/", group$Modalidade,".xlsx")
    } else {
      filename <- paste0(filename, "comb/", paste0(group[!is.na(group)], collapse = '-'),".xlsx")
    }
    
    if (!file.exists(filename)) {
      wb <- createWorkbook(type="xlsx")
      lapply(alpha_mods, FUN = function(mod) {
        write_alpha_in_workbook(mod$mod, wb, mod$factor, mod$factor)
      })
      xlsx::saveWorkbook(wb, filename)
    }
    
    do.call(rbind, lapply(alpha_mods, FUN = function(alpha_mod) {
      cbind('Unidade'=group$Unidade,'Nivel'=group$Nivel, 'Modalidade'=group$Modalidade
            , 'n'=nrow(sdat), 'factor'=alpha_mod$factor, round(alpha_mod$mod$total,2))
    }))
  }
}))

# write summaries of reliability tests
write_csv(reliability_df, 'report/reliability/summary-all.csv')

idx <- !is.na(reliability_df$Unidade) & is.na(reliability_df$Nivel) & is.na(reliability_df$Modalidade) 
write_csv(select(reliability_df[idx,], -starts_with('Nivel'), -starts_with('Modalidade')), 'report/reliability/summary-by-unidade.csv')

idx <- is.na(reliability_df$Unidade) & !is.na(reliability_df$Nivel) & is.na(reliability_df$Modalidade)
write_csv(select(reliability_df[idx,], -starts_with('Unidade'), -starts_with('Modalidade')), 'report/reliability/summary-by-nivel.csv')

idx <- is.na(reliability_df$Unidade) & is.na(reliability_df$Nivel) & !is.na(reliability_df$Modalidade)
write_csv(select(reliability_df[idx,], -starts_with('Unidade'), -starts_with('Nivel')), 'report/reliability/summary-by-modalidade.csv')



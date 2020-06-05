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
                             , DispositivoEmCasa=c(NA,unique(dat$DispositivoEmCasa))
                             , stringsAsFactors = F))
reliability_df <- do.call(rbind, lapply(seq(1,nrow(groups)), FUN = function(i) {
  sdat <- dat; group <- groups[i,]
  if (!is.na(group$Unidade)) sdat <- sdat[sdat$Unidade == group$Unidade,]
  if (!is.na(group$Nivel)) sdat <- sdat[sdat$Nivel == group$Nivel,]
  if (!is.na(group$Modalidade)) sdat <- sdat[sdat$Modalidade == group$Modalidade,]
  if (!is.na(group$DispositivoEmCasa)) sdat <- sdat[sdat$DispositivoEmCasa == group$DispositivoEmCasa,]
  sdat <- select(sdat, starts_with('Item'))
  
  if (nrow(sdat) > 30) {
    alpha_mods <- list(
      'all'=list(factor='all', mod=psych::alpha(sdat))
      , 'ML1'=list(factor='ML1', mod=psych::alpha(sdat[,c('Item11','Item12','Item13')]))
      , 'ML2'=list(factor='ML2', mod=psych::alpha(sdat[,c('Item17','Item18','Item19')]))
      , 'ML3'=list(factor='ML3', mod=psych::alpha(sdat[,c('Item20','Item21')]))
      , 'ML4'=list(factor='ML4', mod=psych::alpha(sdat[,c('Item8','Item9','Item10')]))
      , 'ML5'=list(factor='ML5', mod=psych::alpha(sdat[,c('Item14','Item15','Item16')]))
      , 'ML6'=list(factor='ML6', mod=psych::alpha(sdat[,c('Item22','Item23')]))
      , 'multimodal'=list(factor='multimodal', mod=psych::alpha(sdat[,c('Item8','Item9','Item10','Item11','Item12','Item13','Item14','Item15','Item16','Item20','Item21','Item22','Item23')]))
      , '2nd-mdl1-all'=list(factor='2nd-mdl1-all', mod=psych::alpha(sdat))
      , '2nd-mdl1-INF'=list(factor='2nd-mdl1-INF', mod=psych::alpha(sdat[,c('Item8','Item9','Item10','Item11','Item12','Item13','Item14','Item15','Item16','Item17','Item18','Item19')]))
      , '2nd-mdl1-EXP'=list(factor='2nd-mdl1-EXP', mod=psych::alpha(sdat[,c('Item20','Item21','Item22','Item23')]))
      , '2nd-mdl2-all'=list(factor='2nd-mdl2-all', mod=psych::alpha(sdat[,c('Item8','Item9','Item10','Item11','Item12','Item13','Item14','Item15','Item16','Item20','Item21','Item22','Item23')]))
      , '2nd-mdl2-INF'=list(factor='2nd-mdl2-INF', mod=psych::alpha(sdat[,c('Item8','Item9','Item10','Item11','Item12','Item13','Item14','Item15','Item16')]))
      , '2nd-mdl2-EXP'=list(factor='2nd-mdl2-EXP', mod=psych::alpha(sdat[,c('Item20','Item21','Item22','Item23')]))
    )
    # write reliability analysis
    filename <- "report/reliability/"
    if (is.na(group$Unidade) & is.na(group$Nivel) & is.na(group$Modalidade) & is.na(group$DispositivoEmCasa)) {
      filename <- paste0(filename, "full-all.xlsx")
    } else if (!is.na(group$Unidade) & is.na(group$Nivel) & is.na(group$Modalidade) & is.na(group$DispositivoEmCasa)) {
      filename <- paste0(filename, "by-unidade/", group$Unidade,".xlsx")
    } else if (is.na(group$Unidade) & !is.na(group$Nivel) & is.na(group$Modalidade) & is.na(group$DispositivoEmCasa)) {
      filename <- paste0(filename, "by-nivel/", group$Nivel,".xlsx")
    } else if (is.na(group$Unidade) & is.na(group$Nivel) & !is.na(group$Modalidade) & is.na(group$DispositivoEmCasa)) {
      filename <- paste0(filename, "by-modalidade/", group$Modalidade,".xlsx")
    } else if (is.na(group$Unidade) & is.na(group$Nivel) & is.na(group$Modalidade) & !is.na(group$DispositivoEmCasa)) {
      filename <- paste0(filename, "by-dispositivo-em-casa/", group$DispositivoEmCasa,".xlsx")
    } else {
      filename <- paste0(filename, "comb/", paste0(group[!is.na(group)], collapse = '-'),".xlsx")
    }
    wb <- createWorkbook(type="xlsx")
    lapply(alpha_mods, FUN = function(mod) {
      write_alpha_in_workbook(mod$mod, wb, mod$factor, mod$factor)
    })
    xlsx::saveWorkbook(wb, filename)
    
    do.call(rbind, lapply(alpha_mods, FUN = function(alpha_mod) {
      cbind('Unidade'=group$Unidade,'Nivel'=group$Nivel, 'Modalidade'=group$Modalidade, 'DispositivoEmCasa'=group$DispositivoEmCasa
            , 'n'=nrow(sdat), 'factor'=alpha_mod$factor, round(alpha_mod$mod$total,2))
    }))
  }
}))

# write summaries of reliability tests
write_csv(reliability_df, 'report/reliability/summary-all.csv')

idx <- !is.na(reliability_df$Unidade) & is.na(reliability_df$Nivel) & is.na(reliability_df$Modalidade) & is.na(reliability_df$DispositivoEmCasa) 
write_csv(select(reliability_df[idx,], -starts_with('Nivel'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa')), 'report/reliability/summary-by-unidade.csv')

idx <- is.na(reliability_df$Unidade) & !is.na(reliability_df$Nivel) & is.na(reliability_df$Modalidade) & is.na(reliability_df$DispositivoEmCasa)
write_csv(select(reliability_df[idx,], -starts_with('Unidade'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa')), 'report/reliability/summary-by-nivel.csv')

idx <- is.na(reliability_df$Unidade) & is.na(reliability_df$Nivel) & !is.na(reliability_df$Modalidade) & is.na(reliability_df$DispositivoEmCasa)
write_csv(select(reliability_df[idx,], -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('DispositivoEmCasa')), 'report/reliability/summary-by-modalidade.csv')

idx <- is.na(reliability_df$Unidade) & is.na(reliability_df$Nivel) & is.na(reliability_df$Modalidade) & !is.na(reliability_df$DispositivoEmCasa)
write_csv(select(reliability_df[idx,], -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade')), 'report/reliability/summary-by-dispositivo-em-casa.csv')



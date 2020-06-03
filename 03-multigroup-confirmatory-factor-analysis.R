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

library(MVN)
library(daff)
library(robustHD)

fdat <- read_csv('data/data-fa.csv')
ugroups <- read_csv('data/user-groups.csv')
dat <- merge(ugroups, fdat)

mdl <- '
WLS1 =~ Item9+Item10+Item12+Item13
WLS4 =~ Item15+Item16
WLS3 =~ Item22+Item23
WLS2 =~ Item18+Item19
WLS5 =~ Item20+Item21
WLS2 ~~ 0*WLS1
WLS2 ~~ 0*WLS4
WLS2 ~~ 0*WLS3
WLS2 ~~ 0*WLS5
'

groups <- unique(expand.grid(Unidade=c(NA,unique(dat$Unidade))
                             , Nivel=c(NA,unique(dat$Nivel))
                             , Modalidade=c(NA,unique(dat$Modalidade))
                             , stringsAsFactors = F))
cfa_df <- do.call(rbind, lapply(seq(1,nrow(groups)), FUN = function(i) {
  sdat <- dat; group <- as.list(groups[i,])
  if (!is.na(group$Unidade)) sdat <- sdat[sdat$Unidade == group$Unidade,]
  if (!is.na(group$Nivel)) sdat <- sdat[sdat$Nivel == group$Nivel,]
  if (!is.na(group$Modalidade)) sdat <- sdat[sdat$Modalidade == group$Modalidade,]
  sdat <- select(sdat, starts_with('Item'))
  if (nrow(sdat) > 30) {
    tryCatch(fit <-  cfa(mdl, data = sdat, estimator = 'WLS'), error = function(e) NA)
    return(data.frame(
      "Unidade" = group[['Unidade']]
      , "Nivel" = group[['Nivel']]
      , "Modalidade" = group[['Modalidade']]
      , "n" = nrow(sdat)
      , "df" = tryCatch(fitMeasures(fit, "df"), error = function(e) NA)
      , "chisq" = tryCatch(fitMeasures(fit, "chisq"), error = function(e) NA)
      , "AGFI" = tryCatch(fitMeasures(fit, "agfi"), error = function(e) NA)
      , "TLI" = tryCatch(fitMeasures(fit, "tli"), error = function(e) NA)
      , "CFI" = tryCatch(fitMeasures(fit, "cfi"), error = function(e) NA)
      , "SRMR" = tryCatch(fitMeasures(fit, "srmr"), error = function(e) NA)
      , "RMSEA" = tryCatch(fitMeasures(fit, "rmsea"), error = function(e) NA)
      , "CI.lwr" = tryCatch(fitMeasures(fit, "rmsea.ci.lower"), error = function(e) NA)
      , "CI.upr" = tryCatch(fitMeasures(fit, "rmsea.ci.upper"), error = function(e) NA)
      , "p" = tryCatch(fitMeasures(fit, "rmsea.pvalue"), error = function(e) NA)
    ))
  }
}))
cfa_df <- cfa_df[!is.na(cfa_df$p),]
df.cfa <- cfa_df[!is.na(cfa_df$TLI) & !is.na(cfa_df$CFI),] 
df.cfa <- rbind(df.cfa, cfa_df[is.na(cfa_df$TLI),])
df.cfa <- rbind(df.cfa, cfa_df[is.na(cfa_df$CFI),])
df.cfa <- unique(df.cfa)

# report for all
df.cfa.round <- cbind(
  df.cfa[,c('Unidade','Nivel','Modalidade')]
  , round(select(df.cfa, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade')), 4)
)
write_csv(df.cfa.round, 'report/cfa/mgcfa-all.csv')

# report for unidades
idx.unidades <- !is.na(df.cfa.round$Unidade) & is.na(df.cfa.round$Nivel) & is.na(df.cfa.round$Modalidade) 
df.cfa.round.unidades <- cbind(
  'Unidade'=df.cfa.round[idx.unidades, c('Unidade')]
  , select(df.cfa.round, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'))[idx.unidades,]
)
write_csv(df.cfa.round.unidades, 'report/cfa/mgcfa-unidades.csv')

# report for nivel
idx.nivel <- is.na(df.cfa.round$Unidade) & !is.na(df.cfa.round$Nivel) & is.na(df.cfa.round$Modalidade) 
df.cfa.round.nivel <- cbind(
  'Nivel'=df.cfa.round[idx.nivel, c('Nivel')]
  , select(df.cfa.round, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'))[idx.nivel,]
)
write_csv(df.cfa.round.nivel, 'report/cfa/mgcfa-nivel.csv')

# report for modalidade
idx.modalidade <- is.na(df.cfa.round$Unidade) & is.na(df.cfa.round$Nivel) & !is.na(df.cfa.round$Modalidade) 
df.cfa.round.modalidade <- cbind(
  'Modalidade'=df.cfa.round[idx.modalidade, c('Modalidade')]
  , select(df.cfa.round, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'))[idx.modalidade,]
)
write_csv(df.cfa.round.modalidade, 'report/cfa/mgcfa-modalidade.csv')


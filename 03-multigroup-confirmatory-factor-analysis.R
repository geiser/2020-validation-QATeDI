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
WLS1 =~ Item14+Item15+Item16
WLS2 =~ Item17+Item18+Item19
WLS3 =~ Item8+Item9+Item10
WLS4 =~ Item20+Item21
WLS5 =~ Item22+Item23
WLS2 ~~ 0*WLS1
WLS2 ~~ 0*WLS3
WLS2 ~~ 0*WLS4
WLS2 ~~ 0*WLS5
'

groups <- unique(expand.grid(Unidade=c(NA,unique(dat$Unidade))
                             , Nivel=c(NA,unique(dat$Nivel))
                             , Modalidade=c(NA,unique(dat$Modalidade))
                             , DispositivoEmCasa=c(NA,dat$DispositivoEmCasa))
                             , stringsAsFactors = F)
cfa_df <- as.data.frame(do.call(rbind, lapply(seq(1,nrow(groups)), FUN = function(i) {
  sdat <- dat; group <- groups[i,]
  if (!is.na(group$Unidade)) sdat <- sdat[sdat$Unidade == group$Unidade,]
  if (!is.na(group$Nivel)) sdat <- sdat[sdat$Nivel == group$Nivel,]
  if (!is.na(group$Modalidade)) sdat <- sdat[sdat$Modalidade == group$Modalidade,]
  if (!is.na(group$DispositivoEmCasa)) sdat <- sdat[sdat$DispositivoEmCasa == group$DispositivoEmCasa,]
  sdat <- select(sdat, starts_with('Item'))
  if (nrow(sdat) > 30) {
    fit <- tryCatch(fitMeasures(cfa(mdl, data = sdat, std.lv=T)), error = function(e) NULL)
    if (!is.null(fit)) {
      return(cbind(
        "Unidade" = as.character(group[['Unidade']])
        , "Nivel" = as.character(group[['Nivel']])
        , "Modalidade" = as.character(group[['Modalidade']])
        , "DispositivoEmCasa" = as.character(group[['DispositivoEmCasa']])
        , "n" = nrow(sdat)
        , t(round(as.data.frame(fit), 4))
        , "cfi.obs" = ifelse(fit[['cfi']] < 0.85, 'unacceptable model fit', NA)
        , "tli.obs" = ifelse(fit[['tli']] < 0.85, 'unacceptable model fit', NA)
        , "rmsea.obs" = ifelse(fit[['rmsea']] > 0.10, 'poor model fit', NA)
        , "rmsea.pvalue.obs" = ifelse(fit[['rmsea.pvalue']] < 0.5, 'model fit is worse', NA)
      ))
    }
  }
})))

# report cfa for all
write_csv(cfa_df, 'report/cfa/mgcfa-all.csv')


# report for unidades
idx <- !is.na(cfa_df$Unidade) & is.na(cfa_df$Nivel) & is.na(cfa_df$Modalidade) & is.na(cfa_df$DispositivoEmCasa)
cfa_df.unidades <- cbind(
  'Unidade'=cfa_df[idx, c('Unidade')]
  , select(cfa_df, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa'))[idx,]
)
write_csv(cfa_df.unidades, 'report/cfa/mgcfa-unidades.csv')

# report for nivel
idx <- is.na(cfa_df$Unidade) & !is.na(cfa_df$Nivel) & is.na(cfa_df$Modalidade) & is.na(cfa_df$DispositivoEmCasa)
cfa_df.nivel <- cbind(
  'Nivel'=cfa_df[idx, c('Nivel')]
  , select(cfa_df, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa'))[idx,]
)
write_csv(cfa_df.nivel, 'report/cfa/mgcfa-nivel.csv')

# report for modalidade
idx <- is.na(cfa_df$Unidade) & is.na(cfa_df$Nivel) & !is.na(cfa_df$Modalidade) & is.na(cfa_df$DispositivoEmCasa)
cfa_df.modalidade <- cbind(
  'Modalidade'=cfa_df[idx, c('Modalidade')]
  , select(cfa_df, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa'))[idx,]
)
write_csv(cfa_df.modalidade, 'report/cfa/mgcfa-modalidade.csv')

# report for EdTec em casa
idx <- is.na(cfa_df$Unidade) & is.na(cfa_df$Nivel) & is.na(cfa_df$Modalidade) & !is.na(cfa_df$DispositivoEmCasa)
cfa_df.dispemcasa <- cbind(
  'DispositivoEmCasa'=cfa_df[idx, c('DispositivoEmCasa')]
  , select(cfa_df, -starts_with('Unidade'), -starts_with('Nivel'), -starts_with('Modalidade'), -starts_with('DispositivoEmCasa'))[idx,]
)
write_csv(cfa_df.dispemcasa, 'report/cfa/mgcfa-emcasa.csv')


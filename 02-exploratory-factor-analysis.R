wants <- c('readr','dplyr','psych','lavaan','ggraph','semPlot','robustHD','GPArotation')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

source('common/excel/write_mvn_in_workbook.R')
source('common/excel/write_kmo_in_workbook.R')
source('common/excel/write_efa_in_workbook.R')
source('common/excel/write_cfa_in_workbook.R')

library(GPArotation)
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

dat <- read_csv('data/data.csv')

# checking assumptions 
(mvn_mod <- mvn(select(dat, starts_with("Item"))))  # As there is not normality, use robust methods
(kmo_mod <- KMO(select(dat, starts_with("Item")))) 

if (kmo_mod$MSA < 0.6) {
  print('KMO é miserável, KMO deve ser maior do que 0.6')
}
for (kmo in kmo_mod$MSAi) {
  if (kmo < 0.5) {
    print(paste0('KMO de valor inaceitavel', names(kmo), ', KMO deve ser maior do que 0.5'))
  }
}

# parallel factorial analysis
png(filename = "report/efa/parallel-analysis1.png", width = 800, height = 500)
parallel1 <- (fa.parallel(select(dat, starts_with("Item")), fm = 'ml', fa = 'fa', cor='poly')) # polychoric correlation for categorical data  
dev.off()
(parallel1$nfact) # nro de fatores sugeridos

# Factorial Analysis
# - we used oblique transformation assuming correlation among factors
# - promax method is used in SPSS
rdat <- dat
(fa_mod1_7 <- fa(select(rdat, starts_with("Item")), nfactors = 7, rotate = "promax", cor = 'poly', fm='ml'))  

# o Item24 é eliminado por não estar associado a nemhum dos 7 fatores (loading < 0.4) 
#         ML1   ML4   ML5   ML2   ML3   ML6   ML7   h2     u2 com
#Item24  0.08  0.20  0.03  0.08  0.00  0.29 -0.09 0.24 0.7611 2.5
rdat <- select(dat, -ends_with("Item24"))

png(filename = "report/efa/parallel-analysis2.png", width = 800, height = 500)
parallel2 <- fa.parallel(select(rdat, starts_with("Item")), fm = 'ml', fa = 'fa', cor='poly')
dev.off()
(parallel2$nfact)

(fa_mod2_7 <- fa(select(rdat, starts_with("Item")), nfactors = 7, rotate = "promax", cor = 'poly', fm='ml'))  
(fa_mod2_6 <- fa(select(rdat, starts_with("Item")), nfactors = 6, rotate = "promax", cor = 'poly', fm='ml'))  

png(filename = "report/efa/loading-diagram.png", width = 400, height = 400)
fa.diagram(fa_mod2_6, main='',sort=T)
dev.off()

## Write results in an Excel Workbook
filename <- "report/efa/summary.xlsx"
wb <- createWorkbook(type="xlsx")
write_mvn_in_workbook(mvn_mod, wb)
write_kmo_in_workbook(kmo_mod, wb)
write_efa_in_workbook(fa_mod1_7, wb, "first-EFA-f6", "With 7 Factors and All Items")
write_efa_in_workbook(fa_mod2_7, wb, "first-EFA-f5", "With 7 Factors and Removing the Item24")
write_efa_in_workbook(fa_mod2_6, wb, "EFA-final", "(Final)")
xlsx::saveWorkbook(wb, filename)

write_csv(rdat, 'data/data-fa.csv')


mdls <- list('multi-mdl'=list(name='multi-mdl', mdl='
ML2 =~ Item17+Item18+Item19
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML3 =~ Item22+Item23
ML6 =~ Item20+Item21
ML5 =~ Item14+Item15+Item16

ML1 ~~ ML2
ML1 ~~ ML3
ML1 ~~ ML4
ML1 ~~ ML5
ML1 ~~ ML6

ML2 ~~ ML3
ML2 ~~ ML4
ML2 ~~ ML5
ML2 ~~ ML6

ML3 ~~ ML4
ML3 ~~ ML5
ML3 ~~ ML6

ML4 ~~ ML5
ML4 ~~ ML6

ML5 ~~ ML6
'), 'efa-mdl'=list(name='efa-mdl', mdl='
ML2 =~ Item17+Item18+Item19
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML3 =~ Item22+Item23
ML6 =~ Item20+Item21
ML5 =~ Item14+Item15+Item16

ML1 ~~ 0*ML2
ML1 ~~ 0*ML5
ML1 ~~ 0*ML6

ML2 ~~ 0*ML3
ML2 ~~ 0*ML4
ML2 ~~ 0*ML5
ML2 ~~ 0*ML6

ML3 ~~ 0*ML4
ML3 ~~ 0*ML5

ML4 ~~ 0*ML5
'), '2nd-order-mdl1'=list(name='2nd-order-mdl1', mdl='
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML5 =~ Item14+Item15+Item16
ML2 =~ Item17+Item18+Item19

ML6 =~ Item20+Item21
ML3 =~ Item22+Item23

INF =~ ML4+ML1+ML5+ML2
EXP =~ ML6+ML3
EXP ~~ INF
'), '2nd-order-mdl2'=list(name='2nd-order-mdl2', mdl='
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML5 =~ Item14+Item15+Item16

ML6 =~ Item20+Item21
ML3 =~ Item22+Item23

INF =~ ML4+ML1+ML5
EXP =~ ML6+ML3
EXP ~~ INF
'), 'orth-mdl'=list(name='orth-mdl', mdl='
ML2 =~ Item17+Item18+Item19
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML3 =~ Item22+Item23
ML6 =~ Item20+Item21
ML5 =~ Item14+Item15+Item16

ML1 ~~ 0*ML2
ML1 ~~ 0*ML3
ML1 ~~ 0*ML4
ML1 ~~ 0*ML5
ML1 ~~ 0*ML6

ML2 ~~ 0*ML3
ML2 ~~ 0*ML4
ML2 ~~ 0*ML5
ML2 ~~ 0*ML6

ML3 ~~ 0*ML4
ML3 ~~ 0*ML5
ML3 ~~ 0*ML6

ML4 ~~ 0*ML5
ML4 ~~ 0*ML6

ML5 ~~ 0*ML6
'))
cfa_mdls <- lapply(mdls, FUN = function(x) {
  cfa_mdl <- cfa(x$mdl, data=rdat, std.lv=T, estimator="MLR", meanstructure=T)
  list(name = x$name, cfa = cfa_mdl, fit = fitMeasures(cfa_mdl))
})

## write summary of CFAs
fits_df <- do.call(rbind, lapply(mdls, FUN = function(x) {
  fit <- cfa_mdls[[x$name]]$fit
  return(
    cbind(name=x$name, as.data.frame(t(round(as.data.frame(fit), 3)))
          , "cfi.obs" = ifelse((fit[['cfi']] < 0.85 | fit[['cfi.robust']] < 0.85), 'unacceptable model fit', NA)
          , "tli.obs" = ifelse((fit[['tli']] < 0.85 | fit[['tli.robust']] < 0.85), 'unacceptable model fit', NA)
          , "rmsea.obs" = ifelse((fit[['rmsea']] > 0.10 | fit[['rmsea.robust']] > 0.10), 'poor model fit', NA)
          , "rmsea.pvalue.obs" = ifelse((fit[['rmsea.pvalue']] > 0.05 | fit[['rmsea.pvalue.robust']] > 0.05), "the model has close fit", NA))
  )
}))
write_csv(cbind('model'=rownames(fits_df), fits_df), "report/efa/cfa-fits-summary.csv")

# write summary of comparison for baseline-model
mdl_names <- fits_df$name[is.na(fits_df$cfi.obs) & is.na(fits_df$tli.obs) & is.na(fits_df$rmsea.obs)]
comparison_df <- do.call(rbind, lapply(lapply(as.data.frame(combn(mdl_names, 2)), function(x) {
  list(name=paste(x[1],x[2],sep=':')
       , comb=x 
       , aov=anova(cfa_mdls[[x[1]]]$cfa, cfa_mdls[[x[2]]]$cfa))
}), FUN=function(x) cbind(name=x$name, mdl=x$comb, x$aov)))
comparison_df <- cbind(mdl=comparison_df[,c('name')]
                       , fits_df[comparison_df$mdl, c('name','chisq','df','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper'
                                                      ,'cfi.robust','tli.robust','rmsea.robust','rmsea.ci.lower.robust','rmsea.ci.upper.robust')]
                       , round(comparison_df[,c('AIC','BIC', 'Chisq diff', 'Df diff')], 3), comparison_df[,c('Pr(>Chisq)')])
write_csv(comparison_df, "report/efa/cfa-comparisons-summary.csv")


## write cfa-fits-details
wb <- createWorkbook(type="xlsx")
lapply(mdls, FUN = function(x) {
  
  filename <- paste0("report/efa/",x$name,".png")
  png(filename = filename, width = 1050, height = 400)
  semPaths(cfa_mdls[[x$name]]$cfa, layout = "tree", rotation = 1, intercepts = F, residuals = F, reorder = F, edge.color = 'black', fade=F)
  dev.off()
  
  filename <- paste0("report/efa/",x$name,"-withloadings.png")
  png(filename = filename, width = 1050, height = 400)
  semPaths(cfa_mdls[[x$name]]$cfa, "std", layout = "tree", rotation = 1, intercepts = F, residuals = F, reorder = F, edge.color = 'black', edge.width=0.01, fade=F)
  dev.off()
  
  write_cfa_in_workbook(cfa_mdls[[x$name]]$cfa, wb, x$name)
})
xlsx::saveWorkbook(wb, "report/efa/cfa-fits-detail.xlsx")




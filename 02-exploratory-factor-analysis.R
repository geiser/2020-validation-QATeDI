wants <- c('readr','dplyr','psych','lavaan','ggraph','semPlot','robustHD','GPArotation')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

source('common/excel/write_kmo_in_workbook.R')
source('common/excel/write_efa_in_workbook.R')

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
    print(paste0('KMO de valor inaceitavel para o ', names(kmo), ', KMO deve ser maior do que 0.6'))
  }
}

# parallel factorial analysis
png(filename = "report/efa/parallel-analysis.png", width = 800, height = 500)
parallel <- (fa.parallel(select(dat, starts_with("Item")), fm = 'wls', fa = 'fa', cor='mixed')) # WLS for non-normal data and polychoric correlation for categorical data  
dev.off()
(parallel$nfact) # nro de fatores sugeridos

# factorial analysis
# we used oblique transformation assuming correlation among factors
# promax method is used in SPSS
(fa_mod <- fa(select(dat, starts_with("Item")), nfactors = 5, rotate = "promax", cor = 'mixed', fm='wls'))  

rdat <- select(dat, -ends_with("Item7"), -ends_with("Item24")) # fator com cross-loading e carga menor de 0.4 é removido
(fa_mod <- fa(select(rdat, starts_with("Item")), nfactors = 5, rotate = "promax", cor = 'mixed', fm='wls'))

png(filename = "report/efa/loading-diagram.png", width = 400, height = 400)
fa.diagram(fa_mod, main='',sort=T)
dev.off()

## Write results in an Excel Workbook
filename <- "report/efa/summary.xlsx"
if (!file.exists(filename)) {
  wb <- createWorkbook(type="xlsx")
  write_kmo_in_workbook(kmo_mod, wb)
  write_efa_in_workbook(fa_mod, wb)
  xlsx::saveWorkbook(wb, filename)
}

if (!file.exists('data/data-fa.csv')) {
  write_csv(rdat, 'data/data-fa.csv')
}




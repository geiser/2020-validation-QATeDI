wants <- c('readr','dplyr','psych','lavaan','ggraph','semPlot','robustHD','GPArotation')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

source('common/excel/write_mvn_in_workbook.R')
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
    print(paste0('KMO de valor inaceitavel', names(kmo), ', KMO deve ser maior do que 0.6'))
  }
}

# parallel factorial analysis
png(filename = "report/efa/parallel-analysis1.png", width = 800, height = 500)
parallel1 <- (fa.parallel(select(dat, starts_with("Item")), fm = 'wls', fa = 'fa', cor='poly')) # WLS for non-normal data and polychoric correlation for categorical data  
dev.off()
(parallel1$nfact) # nro de fatores sugeridos


# Factorial Analysis
# - we used oblique transformation assuming correlation among factors
# - promax method is used in SPSS
rdat <- dat
(fa_mod1_6 <- fa(select(rdat, starts_with("Item")), nfactors = 6, rotate = "promax", cor = 'poly', fm='wls'))  
(fa_mod1_5 <- fa(select(rdat, starts_with("Item")), nfactors = 5, rotate = "promax", cor = 'poly', fm='wls'))  

# é eliminado o Item24 por cargar em um único factor dos 6 disponíveis 
#       WLS1  WLS2  WLS7  WLS3  WLS5  WLS4  WLS6   h2    u2 com
#Item24 -0.05  0.00  0.00 -0.06 -0.01  0.82 0.60 0.40 1.0
#
# quando são usados 5 fatores, o Item24 tem carga baixa nos 5 disponíveis 
#       WLS1  WLS2  WLS7  WLS3  WLS5  WLS4  h2    u2 com
#Item24 -0.01  0.27  0.18  0.32  0.04 0.29 0.71 2.6

rdat <- select(dat, -ends_with("Item24"))

png(filename = "report/efa/parallel-analysis2.png", width = 800, height = 500)
parallel2 <- fa.parallel(select(rdat, starts_with("Item")), fm = 'wls', fa = 'fa', cor='poly')
dev.off()
(parallel2$nfact)

(fa_mod2_6 <- fa(select(rdat, starts_with("Item")), nfactors = 6, rotate = "promax", cor = 'poly', fm='wls'))  
(fa_mod2_5 <- fa(select(rdat, starts_with("Item")), nfactors = 5, rotate = "promax", cor = 'poly', fm='wls'))  

# quando são usados 6 fatores - eliminar Item12 e Item13 por cross-loading
# e Item 11 por ficar como único item com carga para o componente WLS6
#        WLS1  WLS2  WLS3  WLS6  WLS4  WLS5   h2   u2 com
#Item11  0.08  0.01  0.07  0.79 -0.11  0.03 0.74 0.26 1.1
#Item12  0.41  0.02  0.01  0.45  0.11  0.00 0.64 0.36 2.1
#Item13  0.40 -0.02 -0.02  0.48  0.08 -0.01 0.61 0.39 2.0
#
# quando são usados 5 fatores - eliminar Item11 que carrega unicamente em um fator
# e Item12 e Item13 são eliminados também porque dependem do Item11
#        WLS1  WLS2  WLS3  WLS4  WLS5   h2   u2 com
#Item11  0.38  0.05  0.05 -0.22  0.67 0.72 0.28 1.9

rdat <- select(dat, -ends_with("Item24")
               , -ends_with("Item11"), -ends_with("Item12"), -ends_with("Item13"))

png(filename = "report/efa/parallel-analysis3.png", width = 800, height = 500)
parallel3 <- fa.parallel(select(rdat, starts_with("Item")), fm = 'wls', fa = 'fa', cor='poly')
dev.off()
(parallel3$nfact)

(fa_mod3_5 <- fa(select(rdat, starts_with("Item")), nfactors = 5, rotate = "promax", cor = 'poly', fm='wls'))  

## Write results in an Excel Workbook
filename <- "report/efa/summary.xlsx"
wb <- createWorkbook(type="xlsx")
write_mvn_in_workbook(mvn_mod, wb)
write_kmo_in_workbook(kmo_mod, wb)
write_efa_in_workbook(fa_mod1_6, wb, "first-EFA-f6", "With 6 Factors and All Items")
write_efa_in_workbook(fa_mod1_5, wb, "first-EFA-f5", "With 5 Factors and All Items")
write_efa_in_workbook(fa_mod2_5, wb, "second-EFA-f6", "With 6 Factors and Removing the Item24")
write_efa_in_workbook(fa_mod2_5, wb, "second-EFA-f5", "With 5 Factors and Removing the Item24")
write_efa_in_workbook(fa_mod3_5, wb, "EFA-final", "(Final)")
xlsx::saveWorkbook(wb, filename)

write_csv(rdat, 'data/data-fa.csv')

png(filename = "report/efa/loading-diagram.png", width = 400, height = 400)
fa.diagram(fa_mod3_5, main='',sort=T)
dev.off()


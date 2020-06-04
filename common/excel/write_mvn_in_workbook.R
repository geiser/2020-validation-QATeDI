## function to write mvn module as sheet
write_mvn_in_workbook <- function(mvn_mod, wb) {
  library(xlsx)
  library(r2excel)
  
  sheet <- xlsx::createSheet(wb, sheetName = "MVN")
  xlsx.addHeader(wb, sheet, "Multivariate Normality Tests", startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Multivariate normality test statistics", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, mvn_mod$multivariateNormality, startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Univariate normality test statistics", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, mvn_mod$univariateNormality, startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Descriptive statistics", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, mvn_mod$Descriptives, startCol = 1, row.names = T)
  
}


write_cfa_in_workbook <- function(cfa_mod, wb, sheetName = "CFA") {
  library(xlsx)
  library(r2excel)
  
  sheet <- xlsx::createSheet(wb, sheetName = sheetName)
  xlsx.addHeader(wb, sheet, paste0("Confirmatory Factor Analysis -", sheetName), startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Fit Measures for Model", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, round(data.frame(t(data.frame(fitMeasures(cfa_mod)))), 4), startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Parameter Estimates", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(parameterEstimates(cfa_mod, standardized = T)), startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Standardized Model Parameters (lambda)", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(lavInspect(cfa_mod, "std")$lambda), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Standardized Model Parameters (theta)", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(lavInspect(cfa_mod, "std")$theta), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Standardized Model Parameters (psi)", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(lavInspect(cfa_mod, "std")$psi), startCol = 1, row.names = T)
  
}

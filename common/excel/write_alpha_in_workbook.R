## function to write alpha module as sheet
write_alpha_in_workbook <- function(alpha_mod, wb, name, short_name = NULL) {
  library(xlsx)
  library(r2excel)
  
  if (is.null(short_name)) short_name <- paste0(" (", substr(name, 1, 12), ")")
  
  sheetName <- paste0("RelAnlys", ifelse(!is.null(name), short_name, ""))
  sheet <- xlsx::createSheet(wb, sheetName = sheetName)
  xlsx.addHeader(wb, sheet, paste0("Reliability Analysis", ifelse(!is.null(name), paste0(" of ", name))), startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Summary", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, as.data.frame(summary(alpha_mod)), startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Reliability if an item is dropped", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(alpha_mod$alpha.drop), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Item statistics", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(alpha_mod$item.stats), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Frequency of each item response", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, data.frame(alpha_mod$response.freq), startCol = 1, row.names = T)
  
}
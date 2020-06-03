## function to write kmo module as sheet
write_kmo_in_workbook <- function(kmo_mod, wb) {
  library(xlsx)
  library(r2excel)
  
  sheet <- xlsx::createSheet(wb, sheetName = "MSA")
  xlsx.addHeader(wb, sheet, "Measure Sampling Adequacy Using KMO Factor Adequacy", startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  adequacy <- 'unacceptable'
  if (kmo_mod$MSA >= 0.5 && kmo_mod$MSA < 0.6) adequacy <- 'miserable'
  if (kmo_mod$MSA >= 0.6 && kmo_mod$MSA < 0.7) adequacy <- 'mediocre'
  if (kmo_mod$MSA >= 0.7 && kmo_mod$MSA < 0.8) adequacy <- 'middling'
  if (kmo_mod$MSA >= 0.8 && kmo_mod$MSA < 0.9) adequacy <- 'meritorious'
  if (kmo_mod$MSA >= 0.9) adequacy <- 'marvelous'
  xlsx.addTable(wb, sheet, data.frame("OverallMSA"=kmo_mod$MSA, adequacy = adequacy), startCol = 1, row.names = F)
  
  
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "MSA for each item", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet,  t(data.frame(kmo_mod$MSAi)), startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Image matrix", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, kmo_mod$Image, startCol = 1)
}


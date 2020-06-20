## function to write efa module as sheet
write_efa_in_workbook <- function(fa_mod, wb, sheetName = "EFA", title = "") {
  library(xlsx)
  library(r2excel)
  
  sheet <- xlsx::createSheet(wb, sheetName = sheetName)
  xlsx.addHeader(wb, sheet, paste("Exploratory Factor Analysis", title), startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Standardized loadings", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, round(data.frame(unclass(fa_mod$loadings)),4), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Factor correlations", level = 2, startCol = 1)
  r.scores.df <- round(data.frame(unclass(fa_mod$r.scores)),4)
  colnames(r.scores.df) <- colnames(fa_mod$Vaccounted)
  rownames(r.scores.df) <- colnames(fa_mod$Vaccounted)
  xlsx.addTable(wb, sheet, r.scores.df, startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Measures of factor score adequacy", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, round(data.frame(print(fa_mod)), 4), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Item complexity", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, round(data.frame(Value=fa_mod$complexity), 4), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Extra information", level = 2, startCol = 1)
  ext_info <- paste0('Mean item complexity = ', mean(fa_mod$complexity), '\n')
  ext_info <- paste0(ext_info, 'The degrees of freedom for the model are = ', fa_mod$dof, '\n')
  ext_info <- paste0(ext_info, 'The objective function was = ', fa_mod$objective, '\n')
  ext_info <- paste0(ext_info, 'The Chi Square of the model is = ', fa_mod$chi, '\n')
  ext_info <- paste0(ext_info, 'The root mean square of the residuals (RMSR) is = ', fa_mod$rms, '\n')
  ext_info <- paste0(ext_info, 'Tucker Lewis Index of factoring reliability is =  ', fa_mod$TLI, '\n')
  ext_info <- paste0(ext_info, 'RMSEA index =  ', fa_mod$RMSEA[[1]], '\n')
  ext_info <- paste0(ext_info, 'The 90% confidence intervals of RMSEA are lower = ', fa_mod$RMSEA[[2]], ' and upper = ', fa_mod$RMSEA[[3]],'\n')
  ext_info <- paste0(ext_info, 'BIC = ', fa_mod$BIC, '\n')
  xlsx.addParagraph(wb, sheet, value = ext_info, startCol = 1)
}

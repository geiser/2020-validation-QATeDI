## 
write_careless_in_latex <- function(dd, filename, in_title = NULL, append = F) {
  library(Hmisc)
  write("", file = filename, append = append)
  if (!append) {
    write(paste("\\documentclass[6pt,a4paper]{article}"
                ,"\\usepackage[a4paper,margin=0.54cm]{geometry}"
                ,"\\usepackage{longtable}"
                ,"\\usepackage{rotating}"
                ,"\\usepackage{pdflscape}"
                ,"\\usepackage{ctable}"
                ,paste("\\title{Summary of carless responses",in_title,"}")
                , "\\date{}" 
                ,"\\begin{document}", sep = "\n")
          , file = filename, append = T)
    write("", file = filename, append = T)
    write("\\maketitle", file = filename, append = T)
    write("", file = filename, append = T)
  }
  
  write("", file = filename, append = T)
  write("\\begin{landscape}", file = filename, append = T)
  
  if (!is.null(dd$get_data()) && length(dd$get_data()) > 0) {
    latex(
      as.data.frame(dd$get_data())
      , rowname = NULL
      , caption = paste("Summary of careless responses", in_title)
      , size = "scriptsize", longtable = T, ctable=F, landscape = F
      , rowlabel = "", where='!htbp', file = filename, append = T)
  }
  
  write("", file = filename, append = T)
  write("\\end{landscape}", file = filename, append = T)
  
  if (!append) {
    write("", file = filename, append = T)
    write("\\end{document}", file = filename, append = T)
  }
}

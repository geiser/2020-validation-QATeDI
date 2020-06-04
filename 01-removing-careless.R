wants <- c('readr', 'dplyr', 'devtools','latexpdf')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
if (!any(rownames(installed.packages()) %in% c('careless'))) {
  devtools::install_github('ryentes/careless', quiet = T)
}

source('common/latex/write_careless_in_latex.R')

library(daff)
library(readr)
library(dplyr)
library(careless)

resp_withcareless <- read_csv('data/data-withcareless.csv')

n <- length(colnames(resp_withcareless))*2/3
careless_info <- careless::longstring(select(resp_withcareless, -starts_with("UserID")))

(careless <- resp_withcareless[careless_info > n,])
write_csv(careless, 'data/careless.csv')

resp <- resp_withcareless[careless_info <= n & complete.cases(resp_withcareless),]
write_csv(resp, 'data/data.csv')

## write report
render_diff(diff_careless <- diff_data(resp_withcareless, resp), file='report/careless/diff.html')
write_diff(diff_careless, file='report/careless/diff.csv')
write_careless_in_latex(diff_careless, 'report/careless/diff.tex', in_title = '')

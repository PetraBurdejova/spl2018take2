devtools::install_github("lborke/yamldebugger")

library(yamldebugger)
install.packages("formatR")
library(formatR)

allKeywords
"plot" %in% allKeywords


#help(yaml.debugger.init)
d_init = yaml.debugger.init(".", show_keywords = TRUE)


#help(yaml.debugger.get.qnames)
qnames = yaml.debugger.get.qnames(d_init$RootPath)



d_results = yaml.debugger.run(qnames, d_init)

OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")

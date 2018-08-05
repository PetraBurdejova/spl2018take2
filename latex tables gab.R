#regressionresults[[i]]<-model
#regressionresults.first[[i]]<-firstmodel
library(stargazer)
stargazer(regressionresults[[1]], regressionresults[[2]],regressionresults[[3]],regressionresults[[4]],regressionresults[[5]],regressionresults[[6]],regressionresults[[7]], title="Results", align=TRUE)
stargazer(regressionresults.first[[1]], regressionresults.first[[2]],regressionresults.first[[3]],regressionresults.first[[4]],regressionresults.first[[5]],regressionresults.first[[6]],regressionresults.first[[7]], title="Results", align=TRUE)
stargazer(regressionresults[[1]], regressionresults[[2]], title="Results", align=TRUE)

library(xtable)
#<<results=tex>>
#  xtable(ols.ass.first)
#@
ols.ass1<-ols.ass[,1:7]
ols.ass2<-ols.ass[,8:11]
xtable(ols.ass1)
xtable(ols.ass2)
ols.ass.first1<-ols.ass.first[,1:7]
ols.ass.first2<-ols.ass.first[,8:11]
xtable(ols.ass.first1)
xtable(ols.ass.first2)

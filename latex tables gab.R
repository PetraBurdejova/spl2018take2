#regressionresults[[i]]<-model
#regressionresults.first[[i]]<-firstmodel
library(stargazer)
stargazer(regressionresults[[1]], regressionresults[[2]],
          regressionresults[[3]], regressionresults[[4]],
          regressionresults[[5]], regressionresults[[6]],
          regressionresults[[7]], title="Results", align=TRUE)
stargazer(regressionresults.first[[1]], regressionresults.first[[2]],regressionresults.first[[3]],regressionresults.first[[4]],regressionresults.first[[5]],regressionresults.first[[6]],regressionresults.first[[7]], title="Results", align=TRUE)
stargazer(regressionresults[[1]], regressionresults[[2]], title="Results", align=TRUE)


#orig data
stargazer(regressionstargazer[1:7], title="Results of original data",
          dep.var.labels = crimetypes, 
          model.names = F, 
          multicolumn = F,
          df = F)

#bp data
stargazer(regressionstargazer.first[1:7], title="Results of Basic-Power-Transformation",
          dep.var.labels = crimetypes, 
          model.names = F, 
          multicolumn = F)

#log-transformed
stargazer(regressionstargazer.log[1:7], title="Results of log-transformed data",
          dep.var.labels = crimetypes, 
          model.names = F, 
          multicolumn = F,
          df = F)


#spatial
stargazer(regressionstargazer.spa[1:7], title="Results of Spatial Regression",
          dep.var.labels = crimetypes, 
          model.names = F, 
          multicolumn = F,
          df = F)


#poisson
stargazer(regressionstargazer.po[1:7], title="Results of Poisson Regression",
          dep.var.labels = crimetypes, 
          model.names = F, 
          multicolumn = F,
          df = F)


library(xtable)
#<<results=tex>>
#  xtable(ols.ass.first)
#@
ols.ass1<-ols.ass[,1:7]
ols.ass2<-ols.ass[,8:11]
xtable(ols.ass)
xtable(ols.ass2)
ols.ass.first1<-ols.ass.first[,1:7]
ols.ass.first2<-ols.ass.first[,8:11]
xtable(ols.ass.first1)
xtable(ols.ass.first2)


xtable(ols.ass.log)
xtable(ols.ass.spa)


spamodel$LMtest

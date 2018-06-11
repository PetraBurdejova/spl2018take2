#interesting <- c("Assault", "Drug.Arrests", "male.youth", "youth", "lone.parent.families.perc", "avg.income")

plot(agg.2016[, c("Assault", "Drug.Arrests", "male.youth", "youth", "lone.parent.families.perc", "avg.income")])

cor(agg.2016[, c("Assault", "Drug.Arrests", "male.youth", "youth", "lone.parent.families.perc", "avg.income")],method="spearman")

agg.2016$avginc2 <- avg.income*avg.income

reg1 <- lm(Assault ~ Drug.Arrests + male.youth + lone.parent.families.perc + avg.income + avginc2, data = agg.2016)
summary(reg1)
plot(reg1)

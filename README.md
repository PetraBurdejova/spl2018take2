Metainfos:
- Name of QuantLet: HistofCor
  Published in: Psychological diagnostics
  Description: Plots a histogram of correlations to make Simpson's paradox visible.
  Keywords: plot, vizualization, random, histogram, correlation, clustering, confidence-interval,
    mean
  Author: Kai Horstmann [New], Caroline Wehner [New]
meta_names_distribution:
- 1
- 1
- 1
- 1
- 1
QNames_meta:
- HistofCor
QCodes:
- "# install and load packages\nlibraries = c(\"multilevel\", \"data.table\")\nlapply(libraries,
  function(x) if (!(x %in% installed.packages())) {\n  install.packages(x)\n})\nlapply(libraries,
  library, quietly = TRUE, character.only = TRUE)\n\ndata(bh1996) # data set from
  package \"multilevel\" to exemplify the function\n\n# input into function: \n# cluster
  variable: GRP\n# 1. level 1 variable: COHES\n# 2. level 1 variable: LEAD\n\ndata
  \   = bh1996[c(\"GRP\", \"COHES\", \"LEAD\")] #extract relevant variables from data
  frame\nvar1    = \"COHES\"\nvar2    = \"LEAD\"\ncluster = \"GRP\"\n\n# 1. level_2
  variable (i. e. means) is calculated for both level1 variables per cluster \n# 2.
  correlation on level_2 is computed\n# 3. one correlation of both variables on level_1
  is computed per cluster\n# 4. plot of the results\n\nHistOfCor = function(data,
  var1, var2, cluster) {\n  variable.names                        =  c(var1, var2,
  cluster)  # rename to get variables\n  names(data)[names(data) == var1]      = \"lvl1.var1\"\n
  \ names(data)[names(data) == var2]      = \"lvl1.var2\"\n  names(data)[names(data)
  == cluster]   = \"cluster\"\n  dt = data.table(data)  # transform data frame to
  data table\n  \n  # 1. compute means (type of correlation can be changed!) and within
  correlation\n  level1.values.dt = dt[,list(mean1 = mean(lvl1.var1, na.rm = T),\n
  \                             mean2 = mean(lvl1.var2, na.rm = T),\n                              cor.lvl1
  = cor(lvl1.var1, lvl1.var2, use = \"complete.obs\")), \n                        by
  = cluster]\n  \n  # 2. level 2 correlation of means (i. e. of variables on level
  2)\n  cor.lvll2   = with(level1.values.dt, cor.test(mean1, mean2, use = \"complete.obs\"))\n
  \ lvl2.cor.r  = round(cor.lvll2$estimate, 3)\n  lvl2.cor.p  = round(cor.lvll2$p.value,
  3)\n  lvl2.cor.df = round(cor.lvll2$parameter, 3)\n  \n  # 3. level 1 correlation
  across clusters\n  cor.lvll1   = with(dt, cor.test(lvl1.var1, lvl1.var2, use = \"complete.obs\"))\n
  \ lvl1.cor.r  = round(cor.lvll1$estimate, 3)\n  lvl1.cor.p  = round(cor.lvll1$p.value,
  3)\n  lvl1.cor.df = round(cor.lvll1$parameter, 3)\n  \n  # 4. plot \n  plot.new()\n
  \ hist(level1.values.dt$cor.lvl1, breaks = 20, xlim = c(-1, 1),  # takes correlations
  from above\n       main=\"\", xlab=\"\", ylab=\"\")  # main = variable name\n  \n
  \ head.text = bquote(paste(\"Correlation of \", bold(.(var1)), \" and \", bold(.(var2)),
  sep = \" \"))  \n  mtext(side = 3, head.text, line = 2, cex = .8)  # variable for
  which the correlations are produced\n  abline(v = lvl2.cor.r, col = \"red\")  #
  add line lvl2 cor \n  abline(v = lvl1.cor.r, col = \"forestgreen\")  # add line
  lvl1 cor (correlation across clusters)\n  meanofcor = mean(level1.values.dt$cor.lvl1,
  na.rm=T)  \n  abline(v = meanofcor, col = \"blue\") # add line lvl1 cor mean\n  testval.2
  = bquote(paste(\"Level-2 correlartion (means, red): \", italic(\"r \"), \"= \",
  .(lvl2.cor.r), \n                           \", \", italic(\"p\"), \" = \",  .(lvl2.cor.p),
  \", with \", italic(\"df\"), \" = \", \n                           .(lvl2.cor.df),
  \ sep = \"\"))  # add test stat summary\n  mtext(side = 3, testval.2, line = 0,
  cex = .6, at = 0)\n  testval.1 = bquote(paste(\"Level-1 correlation (no cluster
  structure, green): \", italic(\"r \"), \"= \", \n                           .(lvl1.cor.r),
  \", \", italic(\"p\"), \" = \",  .(lvl1.cor.p), \", with \", \n                           italic(\"df\"),
  \" = \", .(lvl1.cor.df),  sep = \"\"))  \n  mtext(side = 3, testval.1, line = 1,
  cex = .6, at = 0)\n  \n  out.data = as.data.frame(level1.values.dt)\n  name1    =
  paste(\"mean\", var1, sep = \"\")\n  name2    = paste(\"mean\", var2, sep = \"\")\n
  \ name.cor = \"corOnLvl1\"\n  colnames(out.data) = c(cluster, name1, name2, name.cor)\n
  \ results = list(out.data, cor.lvll2)\n  results\n}\n\n# get output\n# quartz()\n(result
  = HistOfCor(bh1996, var1 = \"COHES\", var2 = \"LEAD\", cluster = \"GRP\"))"
Metainfo_dnames: q, p, a, d, k
Desc_stats:
- 11 word(s), 57 Character(s)
SG_probs: ''
KeywordsOK:
- plot, random, histogram, correlation, clustering, confidence-interval, mean
KeywordsOK_count: 7.0
KeywordsToReplace:
- vizualization
KeywordsToReplace_count: 1.0
yaml_errors_v: []
q_id_errors_v: []
q_code_exist: yes
wrong_quote_signs: no
possible_pictures: 0.0e+00
Qbadnames: ''
QDescription_words: 11.0
Q_found_software: r

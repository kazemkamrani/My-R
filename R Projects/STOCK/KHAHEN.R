KHAHEN <- read.delim("C:/Users/Kazem Kamrani/Desktop/R/STOCK/KHAHEN.txt")
library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(Rgraphviz)
library(MASS)
library(rbmn)

net <- hc(KHAHEN )

graphviz.plot(net)

learnedNet <- bn.fit(net,KHAHEN )

my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("CLOSE1","CLOSE2", "CLOSE3","CLOSE4","CLOSE5","CLOSE6", "CLOSE7","CLOSE8","CLOSE9", "CLOSE10","CLOSE11","CLOSE12", "CLOSE13","CLOSE14","CLOSE90")
obsnames <- c("CLOSE1","CLOSE2", "CLOSE3","CLOSE4","CLOSE5","CLOSE6", "CLOSE7","CLOSE8","CLOSE9", "CLOSE10","CLOSE11","CLOSE12", "CLOSE13","CLOSE14")

obsval<-c(28390.00,
          27770.00,
          27770.00,
          27770.00,
          27770.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00,
          27750.00)
          


prednames <- setdiff(names, obsnames)


print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval))

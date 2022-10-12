PKERMAN <- read.delim("C:/Users/Kazem Kamrani/Desktop/R/STOCK/PKERMAN.txt")
library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(Rgraphviz)
library(MASS)
library(rbmn)

net <- hc(PKERMAN)

graphviz.plot(net)

learnedNet <- bn.fit(net,PKERMAN)

my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("CLOSE1","CLOSE2", "CLOSE3","CLOSE4","CLOSE5","CLOSE6", "CLOSE7","CLOSE8","CLOSE9", "CLOSE10","CLOSE11","CLOSE12", "CLOSE13","CLOSE14","CLOSE90")
obsnames <- c("CLOSE1","CLOSE2", "CLOSE3","CLOSE4","CLOSE5","CLOSE6", "CLOSE7","CLOSE8","CLOSE9", "CLOSE10","CLOSE11","CLOSE12", "CLOSE13","CLOSE14")

obsval<-c(20880.00,
          21430.00,
          22500.00,
          22030.00,
          21140.00,
          20360.00,
          20000.00,
          20040.00,
          20590.00,
          20520.00,
          20230.00,
          19450.00,
          20280.00,
          21290)
          



prednames <- setdiff(names, obsnames)


print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval))

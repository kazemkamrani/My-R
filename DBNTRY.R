HWEB <- read.delim("~/StockData/HWEB.txt")
library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(Rgraphviz)
library(MASS)
library(rbmn)

DBNnet <- hc(HWEB)
graphviz.plot(DBNnet)

learnedNet <- bn.fit(DBNnet,HWEB)

my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)


names <- c("O","H", "L", "C","E","O1","H1", "L1", "C1","E1","O2","H2", "L2", "C2","E2","O3","H3", "L3", "C3","E3","O4","H4", "L4", "C4","E4","O5","H5", "L5", "C5","E5")
obsnames <- c("O","H", "L", "C","E","O1","H1", "L1", "C1","E1","O2","H2", "L2", "C2","E2","O3","H3", "L3", "C3","E3","O4","H4", "L4", "C4","E4")

obsval1<- c(10220,10300,9870,10017,9890,9910,9941,9517,9705,9830,9929,10190,9880,10154,10190,9713,10555,9713,10296,10377,10447,10661,10195,10375,10229)

prednames <- setdiff(names, obsnames)

print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1))


OUT1 <- condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1)
OUT1$mu

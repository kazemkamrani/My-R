GS <- read.delim("C:/Users/Kazem Kamrani/Desktop/R/Tehran Stock Market/GS.txt")
GS[5] <- lapply(GS[5],as.numeric)
Data1 <- GS
names(Data1)[1] <- "OPEN1"
names(Data1)[2] <- "HIGH1"
names(Data1)[3] <- "LOW1"
names(Data1)[4] <- "CLOSE1"
names(Data1)[5] <- "VOL1"
names(Data1)[6] <- "END1"

Data2 <- Data1[2:2687,]
names(Data2)[1] <- "OPEN2"
names(Data2)[2] <- "HIGH2"
names(Data2)[3] <- "LOW2"
names(Data2)[4] <- "CLOSE2"
names(Data2)[5] <- "VOL2"
names(Data2)[6] <- "END2"

Data3 <- Data1[3:2687,]
names(Data3)[1] <- "OPEN3"
names(Data3)[2] <- "HIGH3"
names(Data3)[3] <- "LOW3"
names(Data3)[4] <- "CLOSE3"
names(Data3)[5] <- "VOL3"
names(Data3)[6] <- "END3"

Data4 <- Data1[4:2687,]
names(Data4)[1] <- "OPEN4"
names(Data4)[2] <- "HIGH4"
names(Data4)[3] <- "LOW4"
names(Data4)[4] <- "CLOSE4"
names(Data4)[5] <- "VOL4"
names(Data4)[6] <- "END4"

Data5 <- Data1[5:2687,]
names(Data5)[1] <- "OPEN5"
names(Data5)[2] <- "HIGH5"
names(Data5)[3] <- "LOW5"
names(Data5)[4] <- "CLOSE5"
names(Data5)[5] <- "VOL5"
names(Data5)[6] <- "END5"

Data6 <- Data1[6:2687,]
names(Data6)[1] <- "OPEN6"
names(Data6)[2] <- "HIGH6"
names(Data6)[3] <- "LOW6"
names(Data6)[4] <- "CLOSE6"
names(Data6)[5] <- "VOL6"
names(Data6)[6] <- "END6"

Data7 <- Data1[7:2687,]
names(Data7)[1] <- "OPEN7"
names(Data7)[2] <- "HIGH7"
names(Data7)[3] <- "LOW7"
names(Data7)[4] <- "CLOSE7"
names(Data7)[5] <- "VOL7"
names(Data7)[6] <- "END7"



LData <- cbind(Data1[1:2681,] , Data2[1:2681,] , Data3[1:2681,] , Data4[1:2681,] , Data5[1:2681,] , Data6[1:2681,] , Data7[1:2681,])

library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(graph)
library(Rgraphviz)
library(parallel)
library(BiocGenerics)
library(MASS)
library(rbmn)


net <- hc(LData)


graphviz.plot(net)

learnedNet <- bn.fit(net,LData)
learnedNet
my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("OPEN1","HIGH1","LOW1","CLOSE1","VOL1","END1","OPEN2","HIGH2","LOW2","CLOSE2","VOL2","END2","OPEN3","HIGH3","LOW3","CLOSE3","VOL3","END3","OPEN4","HIGH4","LOW4","CLOSE4","VOL4","END4","OPEN5","HIGH5","LOW5","CLOSE5","VOL5","END5","OPEN6","HIGH6","LOW6","CLOSE6","VOL6","END6","OPEN7","HIGH7","LOW7","CLOSE7","VOL7","END7")

obsnames <- c("OPEN1","HIGH1","LOW1","CLOSE1","VOL1","END1","OPEN2","HIGH2","LOW2","CLOSE2","VOL2","END2","OPEN3","HIGH3","LOW3","CLOSE3","VOL3","END3","OPEN4","HIGH4","LOW4","CLOSE4","VOL4","END4","OPEN5","HIGH5","LOW5","CLOSE5","VOL5","END5","OPEN6","HIGH6","LOW6","CLOSE6","VOL6","END6")

  
obsval<-c(49021.00,
          49021.00,
          49000.00,
          49021.00,
          3305644,
          49021.00,
          51472.00,
          51472.00,
          51472.00,
          51472.00,
          2449763,
          51472.00,
          54045.00,
          54045.00,
          49200.00,
          52260.00,
          11811095,
          53390.00,
          52899.00,
          53000.00,
          49647.00,
          50196.00,
          11276358,
          49647.00,
          49500.00,
          52500.00,
          47687.00,
          48689.00,
          15831794,
          47713.00,
          46500.00,
          48950.00,
          46500.00,
          47378.00,
          8067159,
          46950.00)

prednames <- setdiff(names, obsnames)

print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval))



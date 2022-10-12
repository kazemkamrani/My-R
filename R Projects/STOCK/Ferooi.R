Froi <- read.csv("C:/Users/Kazem Kamrani/Desktop/R/STOCK/Froi.txt", sep="")
CloseData <- Froi
library(plyr)
CountData <- count(CloseData$CLOSE)
NumData <- sum(CountData$freq)

Data1 <- as.data.frame( CloseData [1:NumData,])
names(Data1)[1] <- "Close1"

Data2 <- as.data.frame( CloseData [2:NumData,])
names(Data2)[1] <- "Close2"

Data3 <- as.data.frame( CloseData [3:NumData,])
names(Data3)[1] <- "Close3"

Data4 <- as.data.frame( CloseData [4:NumData,])
names(Data4)[1] <- "Close4"

Data5 <- as.data.frame( CloseData [5:NumData,])
names(Data5)[1] <- "Close5"

Data6 <- as.data.frame( CloseData [6:NumData,])
names(Data6)[1] <- "Close6"

Data7 <- as.data.frame( CloseData [7:NumData,])
names(Data7)[1] <- "Close7"

Data8 <- as.data.frame( CloseData [8:NumData,])
names(Data8)[1] <- "Close8"

Data9 <- as.data.frame( CloseData [9:NumData,])
names(Data9)[1] <- "Close9"

Data10 <- as.data.frame( CloseData [10:NumData,])
names(Data10)[1] <- "Close10"

Data11 <- as.data.frame( CloseData [11:NumData,])
names(Data11)[1] <- "Close11"

Data12 <- as.data.frame( CloseData [12:NumData,])
names(Data12)[1] <- "Close12"

Data13 <- as.data.frame( CloseData [13:NumData,])
names(Data13)[1] <- "Close13"

Data14 <- as.data.frame( CloseData [14:NumData,])
names(Data14)[1] <- "Close14"

Data90 <- as.data.frame( CloseData [104:NumData,])
names(Data90)[1] <- "Close90"

NFD <- NumData - 104

LeanringData <- as.data.frame( cbind(Data1[1:NFD,] , Data2[1:NFD,] , Data3[1:NFD,] , Data4[1:NFD,] , Data5[1:NFD,] , Data6[1:NFD,] , Data7[1:NFD,] , Data8[1:NFD,], Data9[1:NFD,] , Data10[1:NFD,] , Data11[1:NFD,] , Data12[1:NFD,] , Data13[1:NFD,] , Data14[1:NFD,] , Data90[1:NFD,]))
names(LeanringData)[1] <- "Close1"
names(LeanringData)[2] <- "Close2"
names(LeanringData)[3] <- "Close3"
names(LeanringData)[4] <- "Close4"
names(LeanringData)[5] <- "Close5"
names(LeanringData)[6] <- "Close6"
names(LeanringData)[7] <- "Close7"
names(LeanringData)[8] <- "Close8"
names(LeanringData)[9] <- "Close9"
names(LeanringData)[10] <- "Close10"
names(LeanringData)[11] <- "Close11"
names(LeanringData)[12] <- "Close12"
names(LeanringData)[13] <- "Close13"
names(LeanringData)[14] <- "Close14"
names(LeanringData)[15] <- "Close90"


library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(Rgraphviz)
library(MASS)
library(rbmn)

LearningNet <- hc(LeanringData)

graphviz.plot(LearningNet)

learnedNet <- bn.fit(LearningNet,LeanringData)

my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("Close1","Close2", "Close3", "Close4","Close5", "Close6", "Close7","Close8", "Close9", "Close10","Close11", "Close12", "Close13", "Close14", "Close90")
obsnames <- c("Close1","Close2", "Close3", "Close4","Close5", "Close6", "Close7","Close8", "Close9", "Close10","Close11", "Close12", "Close13", "Close14")

obsval<-c(CloseData[NumData,1],
          CloseData[NumData-1,1],
          CloseData[NumData-2,1],
          CloseData[NumData-3,1],
          CloseData[NumData-4,1],
          CloseData[NumData-5,1],
          CloseData[NumData-6,1],
          CloseData[NumData-7,1],
          CloseData[NumData-8,1],
          CloseData[NumData-9,1],
          CloseData[NumData-10,1],
          CloseData[NumData-11,1],
          CloseData[NumData-12,1], 
          CloseData[NumData-13,1])

prednames <- setdiff(names, obsnames)
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval))

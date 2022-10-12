bit <- read.delim("C:/Users/Kazem Kamrani/Desktop/bit.txt")

bit[5] <- lapply(bit[5],as.numeric)

LDataW <- bit
Data1 <- LDataW[2:108,]
names(Data1)[1] <- "Open1"
names(Data1)[2] <- "High1"
names(Data1)[3] <- "Low1"
names(Data1)[4] <- "Close1"
names(Data1)[5] <- "Volume1"

Data2 <- LDataW[3:108,]
names(Data2)[1] <- "Open2"
names(Data2)[2] <- "High2"
names(Data2)[3] <- "Low2"
names(Data2)[4] <- "Close2"
names(Data2)[5] <- "Volume2"

Data3 <- LDataW[4:108,]
names(Data3)[1] <- "Open3"
names(Data3)[2] <- "High3"
names(Data3)[3] <- "Low3"
names(Data3)[4] <- "Close3"
names(Data3)[5] <- "Volume3"

Data4 <- LDataW[5:108,]
names(Data4)[1] <- "Open4"
names(Data4)[2] <- "High4"
names(Data4)[3] <- "Low4"
names(Data4)[4] <- "Close4"
names(Data4)[5] <- "Volume4"

Data5 <- LDataW[6:108,]
names(Data5)[1] <- "Open5"
names(Data5)[2] <- "High5"
names(Data5)[3] <- "Low5"
names(Data5)[4] <- "Close5"
names(Data5)[5] <- "Volume5"

Data6 <- LDataW[7:108,]
names(Data6)[1] <- "Open6"
names(Data6)[2] <- "High6"
names(Data6)[3] <- "Low6"
names(Data6)[4] <- "Close6"
names(Data6)[5] <- "Volume6"

Data7 <- LDataW[8:108,]
names(Data7)[1] <- "Open7"
names(Data7)[2] <- "High7"
names(Data7)[3] <- "Low7"
names(Data7)[4] <- "Close7"
names(Data7)[5] <- "Volume7"

Data8 <- LDataW[9:1170,]
names(Data8)[1] <- "Open8"
names(Data8)[2] <- "High8"
names(Data8)[3] <- "Low8"
names(Data8)[4] <- "Close8"
names(Data8)[5] <- "Volume8"

Data9 <- LDataW[10:108,]
names(Data9)[1] <- "Open9"
names(Data9)[2] <- "High9"
names(Data9)[3] <- "Low9"
names(Data9)[4] <- "Close9"
names(Data9)[5] <- "Volume9"

Data10 <- LDataW[11:108,]
names(Data10)[1] <- "Open10"
names(Data10)[2] <- "High10"
names(Data10)[3] <- "Low10"
names(Data10)[4] <- "Close10"
names(Data10)[5] <- "Volume10"

Data11 <- LDataW[12:108,]
names(Data11)[1] <- "Open11"
names(Data11)[2] <- "High11"
names(Data11)[3] <- "Low11"
names(Data11)[4] <- "Close11"
names(Data11)[5] <- "Volume11"

Data12 <- LDataW[13:108,]
names(Data12)[1] <- "Open12"
names(Data12)[2] <- "High12"
names(Data12)[3] <- "Low12"
names(Data12)[4] <- "Close12"
names(Data12)[5] <- "Volume12"

Data13 <- LDataW[14:1710,]
names(Data13)[1] <- "Open13"
names(Data13)[2] <- "High13"
names(Data13)[3] <- "Low13"
names(Data13)[4] <- "Close13"
names(Data13)[5] <- "Volume13"

Data14 <- LDataW[15:108,]
names(Data14)[1] <- "Open14"
names(Data14)[2] <- "High14"
names(Data14)[3] <- "Low14"
names(Data14)[4] <- "Close14"
names(Data14)[5] <- "Volume14"

DataP <- LDataW[16:108,]
names(DataP)[1] <- "OpenP"
names(DataP)[2] <- "HighP"
names(DataP)[3] <- "LowP"
names(DataP)[4] <- "CloseP"
names(DataP)[5] <- "VolumeP"
DataP$VolumeP <- NULL


LDataW <- cbind(LDataW[1:93,] , Data1[1:93,] , Data2[1:93,] , Data3[1:93,], Data4[1:93,] , Data5[1:93,] , Data6[1:93,], Data7[1:93,] , Data8[1:93,] , Data9[1:93,], Data10[1:93,] , Data11[1:93,] , Data12[1:93,], Data13[1:93,] , Data14[1:93,] , DataP)


library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(graph)
library(parallel)
library(BiocGenerics)
library(Rgraphviz)
library(MASS)
library(rbmn)

netW <- hc(LDataW)

graphviz.plot(netW)

learnedNet <- bn.fit(netW,LDataW )

my.bn.par.rbmn <- bnfit2nbn(learnedNet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("Open","High", "Low", "Close","Volume","Open1","High1", "Low1", "Close1","Volume1","Open2","High2", "Low2", "Close2","Volume2","Open3","High3", "Low3", "Close3","Volume3","Open4","High4", "Low4", "Close4","Volume4","Open5","High5", "Low5", "Close5","Volume5","Open6","High6", "Low6", "Close6","Volume6","Open7","High7", "Low7", "Close7","Volume7","Open8","High8", "Low8", "Close8","Volume8","Open9","High9", "Low9", "Close9","Volume9","Open10","High10", "Low10", "Close10","Volume10","Open11","High11", "Low11", "Close11","Volume11","Open12","High12", "Low12", "Close12","Volume12","Open13","High13", "Low13", "Close13","Volume13","Open14","High14", "Low14", "Close14","Volume14","OpenP","HighP", "LowP", "CloseP")
obsnames <- c("Open","High", "Low", "Close","Volume","Open1","High1", "Low1", "Close1","Volume1","Open2","High2", "Low2", "Close2","Volume2","Open3","High3", "Low3", "Close3","Volume3","Open4","High4", "Low4", "Close4","Volume4","Open5","High5", "Low5", "Close5","Volume5","Open6","High6", "Low6", "Close6","Volume6","Open7","High7", "Low7", "Close7","Volume7","Open8","High8", "Low8", "Close8","Volume8","Open9","High9", "Low9", "Close9","Volume9","Open10","High10", "Low10", "Close10","Volume10","Open11","High11", "Low11", "Close11","Volume11","Open12","High12", "Low12", "Close12","Volume12","Open13","High13", "Low13", "Close13","Volume13","Open14","High14", "Low14", "Close14","Volume14","OpenP")

obsval<-c()

prednames <- setdiff(names, obsnames)

print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval))






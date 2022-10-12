library(tidyquant)
library(bnlearn)
library(MASS)
library(rbmn)
library(Rgraphviz)
library(gRbase)
library(gRain)
library(grid)
library(MASS)
library(BiocGenerics)

#Getting Data
df <- read.csv("C:/Users/Kazem Kamrani/Desktop/ForexData/EURUSDMR.csv")

df$Volume <- NULL
df$Time <- NULL
df$Open <- scale(df$Open)



NumData <- COUNT(df$High)
Xp <-df$Open[NumData]
print(Xp) 


#Data Preparation
dfmodel <- df[1:(NumData-1),]
dfmodel$Low <- NULL
dfmodel$Close <- NULL
dfmodel$Date <- NULL


#Model 
mynet  <- empty.graph(nodes = c("Open","High"))
arc.set <- matrix(c("Open","High"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(mynet) <- arc.set

MyPlot=graphviz.plot(mynet)

mybn=bn.fit(mynet,dfmodel)

my.bn.par.rbmn <- bnfit2nbn(mybn)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("Open","High")
obsnames <- c("Open" )

obsval1 <- c(Xp)
            

prednames <- setdiff(names, obsnames)
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1))







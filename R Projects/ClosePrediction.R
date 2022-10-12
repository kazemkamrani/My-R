library(tidyquant)
library(bnlearn)
library(MASS)
library(rbmn)
library(Rgraphviz)
library(gRbase)
library(gRain)
library(grid)
library(MASS)

dfGet <- read.csv("C:/Users/Kazem Kamrani/Desktop/ForexData/EURUSDMR.csv")
dfGet$Time <- NULL
dfGet$Open <- NULL
dfGet$Volume <- NULL

df <- dfGet 
NumData <- COUNT(df$High)
NFD <- (NumData - 1)

#Data Preparation
df1 <- as.data.frame(scale(df$High))
xpH <- df1$V1[NumData]
df1 <- df1[1:NFD,]
df1 <- as.data.frame( df1 )
names(df1)[1] <- "High"

df2 <- as.data.frame(scale(df$Low))
xpL <- df2$V1[NumData]
df2 <- df2[1:NFD,]
df2 <- as.data.frame( df2 )
names(df2)[1] <- "Low"

df3 <- as.data.frame(scale(df$Close))
xpC <- df3$V1[NumData]
df3 <- df3[1:NFD,]
df3 <- as.data.frame( df3 )
names(df3)[1] <- "Close"

df4 <- as.data.frame(df$Close)
df4 <- df4[2:NumData ,]
df4 <- as.data.frame( df4 )
names(df4)[1] <- "ClosePrediction"


LD <- as.data.frame(cbind(df1,df2,df3,df4 ))


#Model 
mynet  <- empty.graph(nodes = c("High","Low","Close","ClosePrediction" ))
arc.set <- matrix(c("High","ClosePrediction",
                    "Low","ClosePrediction",
                    "Close","ClosePrediction" ),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(mynet) <- arc.set

MyPlot=graphviz.plot(mynet)

mybn=bn.fit(mynet,LD) 

my.bn.par.rbmn <- bnfit2nbn(mybn)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("High","Low","Close","ClosePrediction")
obsnames <- c("High","Low","Close" )

obsval1 <- c(xpH,xpL,xpC)


prednames <- setdiff(names, obsnames)
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1))



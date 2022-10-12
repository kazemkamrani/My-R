D <- read.delim("C:/Users/Kazem Kamrani/Desktop/Shasta.txt")

Y <- D$X.CLOSE.

Ymean <- mean(Y)
Ystdv <- sd(Y)
print(Ymean)
print(Ystdv)

HighValue <- Ymean + 1*Ystdv
LowValue <- Ymean - 1*Ystdv

print(HighValue)
print(LowValue)

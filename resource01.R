mybay <- read.delim("C:/Users/Kazem Kamrani/Desktop/mybay.txt")
library(bnlearn)
library(MASS)
library(rbmn)
library(Rgraphviz)
mynet  <- empty.graph(nodes = c("Re", "De", "Rd", "Gw", "Q", "SDI", "Vns", "Rel", "P", "V", "C"))


arc.set <- matrix(c("Q", "SDI",
                    "Gw", "Vns",
                    "De", "Vns",
                    "Re", "Vns",
                    "Re", "Rel",
                    "De", "Rel", 
                    "SDI", "P",
                    "Vns", "V",
                    "Rel", "C",
                    "P", "Rd",
                    "V", "Rd",
                    "C", "Rd"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(mynet) <- arc.set



MyPlot=graphviz.plot(mynet)


mybn=bn.fit(mynet,mybay)
mybn
my.bn.par.rbmn <- bnfit2nbn(mybn)
print8nbn(my.bn.par.rbmn)
?nbn2mn
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)
 my.bn.par.mn

names <- c("Re", "De", "Rd", "Gw", "Q", "SDI", "Vns", "Rel", "P", "V", "C")
obsnames <- c("Q", "Gw", "De", "Re" )

obsval1 <- c(12.488,4621.40496,2572.536871,9132.48)
obsval2 <- c(24.223,4621.40496,2398.490206,12216.96)
obsval3 <- c(15.279,4621.40496,8876.645744,13520.4)
obsval4 <- c(7.984,4621.40496,17622.61756,7814.88)
obsval5 <- c(17.352,4621.40496,17593.53839,21220.1)
obsval6 <- c(6.155,4621.40496,3026.722222,8311.14)
obsval7 <- c(8.675,4621.40496,2615.031078,7101.216)
obsval8 <- c(10.194,4621.40496,2048.335172,11266.56)
obsval9 <- c(15.469,4621.40496,7386.824026,14726.48)
obsval10 <- c(14.392,4621.40496,14614.62929,10091.52)
obsval11 <- c(24.013,4621.40496,14776.69819,22481.28)
obsval12 <- c(17.794,4621.40496,3154.312895,6514.56)
#obsval13 <- c(17.83,4621.40496,3646.257423,11973.312)

prednames <- setdiff(names, obsnames)

print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval2))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval3))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval4))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval5))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval6))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval7))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval8))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval9))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval10))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval11))
print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval12))
#print8mn(condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval13))



summary(mybay)







library(bnlearn)
library(gRbase)
library(gRain)
library(grid)
library(Rgraphviz)
library(MASS)
library(rbmn)
Baynet <- read.delim("C:/Users/Kazem Kamrani/Desktop/Bayesian Networks with Examples in R/Baynet.txt")

Risknet <-  empty.graph(nodes = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"))
arc.set <- matrix(c("As", "HEM",
                    "Hg", "HEM",
                    "Pb", "HEM",
                    "Cd", "HEM",
                    "HEM", "E5",
                    "HEM", "E3",
                    "NO3", "E3",
                    "NO3", "E5",
                    "TSS", "E4",
                    "TSS", "E6",
                    "ALK", "E4",
                    "ALK", "E6",
                    "EC", "E4",
                    "EC", "E6",
                    "SAR", "E4",
                    "SAR", "E11",
                    "TH", "E4",
                    "TH", "E11",
                    "OIL", "E6",
                    "OIL", "E10",
                    "OIL", "E3",
                    "E5", "E10",
                    "DET", "E10",
                    "DET", "E3",
                    "DET", "E7",
                    "SO4", "E3",
                    "SO4", "E10",
                    "PO4", "E3",
                    "PO4", "E10",
                    "BOD5", "E3",
                    "COD", "E3",
                    "FECAL", "E9",
                    "FECAL", "E2",
                    "E2", "E8",
                    "E3", "E8",
                    "E10", "E13",
                    "E4", "E8",
                    "E6", "E11",
                    "H2S", "E1",
                    "E1", "E7",
                    "E11", "E13",
                    "E9", "E13",
                    "E8", "E12",
                    "E7", "E14",
                    "E7", "ER",
                    "E13", "E14",
                    "E13", "ER",
                    "E12", "E14",
                    "E12", "ER",
                    "E12", "T1",
                    "E14", "ER",
                    "E14", "S1",
                    "E14", "ECO2",
                    "ER", "R", 
                    "S1", "S8",
                    "S1", "S7",
                    "S1", "S6",
                    "S4", "S8",
                    "S4", "S7",
                    "S4", "S6",
                    "S2", "S7",
                    "S2", "S6",
                    "S3", "S7",
                    "S3", "S6",
                    "T1", "ECO2",
                    "T1", "ECO4",
                    "T1", "ECO3",
                    "T1", "T4",
                    "T1", "T5",
                    "S8", "S10",
                    "S7", "S10",
                    "S7", "S9",
                    "S6", "S9",
                    "S6", "ECO6",
                    "S6", "ECO4",
                    "ECO1", "ECO6",
                    "ECO2", "ECO6",
                    "ECO2", "ECO4",
                    "ECO2", "ECO3",
                    "S5", "S10",
                    "S5", "ECO6",
                    "S5", "ECO4",
                    "S5", "ECO3",
                    "S5", "T4",
                    "S5", "T5",
                    "T2", "ECO6",
                    "T2", "ECO4",
                    "T2", "ECO3",
                    "T2", "T4",
                    "T2", "T5",
                    "T3", "ECO4",
                    "T3", "ECO3",
                    "T3", "T4",
                    "T3", "T5",
                    "S10", "SR",
                    "S9", "SR",
                    "ECO6", "ECOR",
                    "ECO4", "ECO5",
                    "ECO3", "ECO5",
                    "T4", "TR",
                    "T5", "TR",
                    "T6", "TR",
                    "TR", "R",
                    "ECO5", "ECOR",
                    "ECOR", "R",
                    "SR", "R"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(Risknet) <- arc.set

graphviz.plot(Risknet)

RLnet <- bn.fit(Risknet, Baynet )


my.bn.par.rbmn <- bnfit2nbn(RLnet)
print8nbn(my.bn.par.rbmn)
my.bn.par.mn <- nbn2mn(my.bn.par.rbmn)

names <- c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R")
obsnames <- c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"S2",	"S3",	"S4",	"S5",	"ECO1",	"T2",	"T3",	"T6")


obsval1 <- c(0.1,12,38.9,19.7,2,9.1,1.1,0.1,0.9,1.5,0.1,110,249,219.1,3.4,1149,0.3,0.8,10,10,50,50,50,30,10,30)
obsval2 <- c(0.8,46,159.4,34.4,29,1.1,1.3,0.1,0.8,7.5,0.3,130,255,325.2,2.9,1293,0.5,0.8,10,10,50,50,50,30,10,30)
obsval3 <- c(0.1,42,68.2,21.3,2,1.1,1.4,0,0.2,5.1,0.3,130,304,260.9,3.1,1335,0.5,0.9,10,10,50,50,50,30,10,30)
obsval4 <- c(0.2,43,109.6,29.1,2,1.1,1.4,0,0,2.1,0,136,290,374.8,4.5,879,0.4,0.9,10,10,50,50,30,50,10,30)
obsval5 <- c(1.3,21.8,37.4,32.3,220,11.7,3.2,1,0.6,0,0,125,274,295,3.4,1487,0.5,0.9,30,10,50,50,50,30,10,50)
obsval6 <- c(1.4,10,30,8,400,40,2.7,0,0,0,0,92.2,557,460,0.7,830,0.5,2.6,30,10,30,70,30,50,10,30)
obsval7 <- c(1,60,87,60,1100,71,5.6,0.2,0.7,50,10,447,202,520,2.5,2780,20,2.5,50,50,10,30,70,70,50,70)
obsval8 <- c(0.7,100,110,23,1100,84,4.8,0.2,3.3,20,40,427,204,472,3.6,2290,11.5,4.7,50,50,10,30,70,70,50,70)
obsval9 <- c(0.3,90,105,30,1100,60,14,4.3,9.1,50,23,442,214,690,3.8,2620,22,4.9,50,50,10,30,70,70,50,90)
obsval10 <- c(0.6,140,242,544.9,460,8.6,2.1,15.7,2.9,3.1,1.9,449,222,381,8.3,1534,0.3,1,30,30,70,10,30,10,30,50)
obsval11 <- c(0.1,21,15,12,28,123,10,2,0.2,21,0,172,834,400,0.3,2214,1.1,2.1,50,10,30,70,90,30,10,30)
obsval12 <- c(0.2,56,23,16,23,85,17,2.5,1,16,0,245,537,320,0.8,1776,0.8,1.7,30,10,30,70,70,10,10,30)
obsval13 <- c(0.3,32,124,31,49,106,2,0.6,0.2,5,3.4,326,771,380,0.5,2113,2,3,50,10,30,50,90,30,10,10)
obsval14 <- c(0.9,26,35,7,21,106,2,3,0.6,26,0,168,672,410,0.5,2014,2.4,2.2,30,10,30,50,90,30,10,10)
obsval15 <- c(1.4,46,87,18,39,87,14,0.5,0.1,19,1.6,369,615,412,0.9,2016,0.9,2.5,30,10,30,30,90,30,10,10)
obsval16 <- c(0.8,22,45,14,36,114,2,1,1,22,0,246,638,415,0.8,2014,1.2,1.9,30,10,30,50,70,30,10,10)
obsval17 <- c(1.2,55,43,23,16,75,11,3.6,2,8,0,348,535,300,1,1796,0.7,2.2,30,10,30,30,70,30,10,10)
obsval18 <- c(1,45,87,54,36,89,14,1.3,1.2,17,3.1,289,682,319,0.7,2112,0.9,3.4,30,10,30,70,90,10,10,10)
obsval19 <- c(0.9,46,78,11,38,79,11,2.1,2,6,0,225,630,300,0.8,1883,2.9,3.8,30,10,30,70,70,10,10,10)
obsval20 <- c(0.1,56,91,175,245700,3.7,8.5,0.1,1,0.1,0.4,305,349,181,4.5,1383,3.6,2.6,50,70,90,90,90,70,70,70)
obsval21 <- c(0.2,112,145,235,111810,60.5,18.4,0,0,0,0,391,441,303.5,4.2,2020,2.6,2,50,70,90,90,90,90,90,70)
obsval22 <- c(2,82.5,118.6,205,380000,43.6,12.2,0,0.1,0.1,0,424,383,236.2,4.4,1730.8,2,1.6,50,70,90,90,90,70,70,90)
obsval23 <- c(0.1,35,77,68,60,14,1.7,5,0.1,0.1,0.1,100,471,584,7.6,2505,7.4,2,10,50,90,90,50,90,10,50)
obsval24 <- c(0.5,69.1,139.5,89.3,502,27.4,3.2,0,0,0.8,0,416,167,292.8,2.8,2583,8.4,1.5,10,10,50,30,30,10,30,70)
obsval25 <- c(0.1,45,100,14,18,3,3.3,0,1.9,5.1,0,491,402,280.2,4.8,1110,0.5,0.6,70,50,70,70,50,50,70,90)
obsval26 <- c(1.6,246,436,45,2000,22,2.1,0.5,0.5,16,1.2,521,216,349.4,6.6,506,1.2,0.1,50,70,50,70,70,50,50,90)
obsval27 <- c(0.7,48,108,53,410,3.8,3.7,6.1,1.5,1.1,1,522,353,270.2,0.9,1006,1.6,1.2,30,50,70,50,70,50,70,90)
obsval28 <- c(0,41,87,9,10,8,7.7,11.7,4.1,2.1,2.9,487,352,385.2,5,1807,1.9,1.4,10,10,30,50,50,30,10,30)
obsval29 <- c(0.1,28,60.7,21,48,2,3,1,0,0,0,503,310,325.2,4.5,1529.7,5,1,70,50,90,50,50,50,50,70)
obsval30 <- c(2,10,19.1,29,210,1.6,0.8,0.1,0,1.2,0,499,334,420.2,2.3,1780,16,3.1,50,70,50,50,50,50,70,70)
obsval31 <- c(1.1,29,62,99,67,2,3.7,9.8,3.2,1.7,1.9,532,281,385.2,6.2,1879,3.1,1.3,30,50,70,50,50,30,10,70)
obsval32 <- c(0,8.1,14,18,15,3.8,18,6,3,0,0,171.5,475,300.2,2.7,1717,4,1.2,10,10,50,30,10,10,10,10)
obsval33 <- c(0.1,16.8,32,15,6,11,20,7,3,0,0,182.1,360,310.1,2.9,1722,3.9,1.9,10,10,70,30,10,10,10,30)
obsval34 <- c(0.4,8.1,14,15,6,3.8,34,6,0.3,0,0,171.5,360,300.2,2.7,1717,2.9,2,10,10,30,30,10,10,10,50)
obsval35 <- c(2.5,107,203,115,1985,46,54,3.4,3.5,0,3,265.7,699,445.1,5.2,2351,1.8,1.8,50,10,30,30,30,10,10,50)
obsval36 <- c(2.3,88,200,163,1220,149.9,8,2.1,1,0,3.8,444.4,864,425.1,5.1,2340,2,2,30,10,50,10,50,10,10,50)
obsval37 <- c(1.8,107,203,163,1985,149.9,7,3.4,3.5,0.1,3.8,444.4,864,445.1,5.2,2351,2.1,0.9,50,10,50,10,50,10,10,50)
obsval38 <- c(2.3,52.5,103.8,61.2,864,24.1,37,2.1,0.8,0.1,0.4,224.1,582,362,3.4,1904,1.1,1.1,30,10,50,30,30,10,10,50)
obsval39 <- c(0.9,49.4,104.7,69.6,378,57.7,28,1.3,0.8,0.2,0.8,256.5,567,348,3.6,1928.1,0.9,1.7,30,10,50,30,30,10,10,50)
obsval40 <- c(1.3,51.3,104.2,64.4,675,37.2,13,1.8,0.8,0,0.6,240.3,576,356.5,3.5,1913.4,0.8,2.1,30,10,50,30,30,10,10,50)
obsval41 <- c(3,83,236,111,1000,9,22.9,2,6.1,60,1.1,310,1290,172.3,2.3,1420,2,1.5,30,30,50,10,10,10,10,50)
obsval42 <- c(0,9,8,7,0,3.6,0.5,0,0,0,0,160,214,96.4,3.1,840,0.1,0.9,10,10,10,30,30,90,10,10)
obsval43 <- c(2.8,32.1,35,25.2,21000,3.4,8.2,10,10,80,20,192,161,257,3.5,380,1.4,1.9,30,10,10,90,50,70,10,10)
obsval44 <- c(0,5,17.5,11,2,188.3,41.4,0,0,0,0,200,150,159,1,4074,3.2,0.7,10,10,10,30,10,90,50,50)
obsval45 <- c(5,23,40,13,11000,2.1,0.5,0,2,50,10,510,2940,152.2,5.5,5050,8.8,0.6,90,50,70,10,30,30,50,70)
obsval46 <- c(2.1,21,38,11,2400,4.1,0.6,1,2,50,10,1050,17100,168.5,22.3,3890,7,0.5,50,70,70,10,50,30,90,90)
obsval47 <- c(4.2,56,93,48,11000,2,0.5,1.1,2,50,10,955,3550,220.2,6,6110,4,1,90,90,50,10,50,10,90,90)
obsval48 <- c(0.1,5,7,25,20,30,32,10,1,10,3,250,200,150,2.9,1000,0.9,0.4,10,10,10,10,90,10,70,50)
obsval49 <- c(0.1,7,10,50,1000,50,28,1.9,6,50,10,400,500,500,3,3000,11.3,1.8,10,10,10,10,90,10,70,50)
obsval50 <- c(0.1,6.2,19.5,16.2,2,19.7,2.8,1.6,0.1,30,27,269,330,365,0.9,605,1.7,0.1,10,30,30,50,70,30,70,50)
obsval51 <- c(0.2,112,145,235,230,60.5,18.4,0.3,0,0,0.5,309,442,336,3.7,2020,6.5,3.4,50,50,30,30,70,30,90,30)
obsval52 <- c(0.4,345,605,82,300,2,10,0.4,0,1.3,0.9,338,338,329,2.5,1054,10,0.5,50,10,30,10,50,10,30,70)
obsval53 <- c(1.8,21.1,35.6,18.6,220,13.6,3.3,0,0,0,0,98,251,405,2.6,1370,2,0.5,50,30,90,10,50,30,50,50)
obsval54 <- c(1.1,35.7,94,26.1,9,3,1.5,0.4,1.3,0.9,0,156,290,441,2.9,1350,6,0.4,30,10,30,50,70,50,50,50)
obsval55 <- c(0,30,60,25,400,10,6,0.1,0,1,0.1,150,200,200,3,1000,10,0.5,10,10,10,10,90,10,70,90)
obsval56 <- c(3.4,24.2,39.9,24.4,220,11.5,5,0.1,0.6,0,0,227,390,380,3.6,1259,0.4,0.8,30,30,70,30,50,70,50,90)
obsval57 <- c(2.9,21,15,397,28,123,10,0,0,0.4,0.2,191,311,373,3.2,2214,1.7,0.5,70,90,90,10,70,50,30,90)
obsval58 <- c(3.1,16,29,20,960,2.9,1.4,1.3,0.3,1.5,0,231,298,400,3.1,616,10,1,30,50,70,10,90,90,90,10)


prednames <- setdiff(names, obsnames)

OUT1=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval1)
OUT2=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval2)
OUT3=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval3)
OUT4=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval4)
OUT5=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval5)
OUT6=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval6)
OUT7=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval7)
OUT8=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval8)
OUT9=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval9)
OUT10=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval10)
OUT11=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval11)
OUT12=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval12)
OUT13=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval13)
OUT14=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval14)
OUT15=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval15)
OUT16=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval16)
OUT17=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval17)
OUT18=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval18)
OUT19=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval19)
OUT20=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval20)
OUT21=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval21)
OUT22=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval22)
OUT23=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval23)
OUT24=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval24)
OUT25=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval25)
OUT26=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval26)
OUT27=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval27)
OUT28=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval28)
OUT29=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval29)
OUT30=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval30)
OUT31=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval31)
OUT32=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval32)
OUT33=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval33)
OUT34=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval34)
OUT35=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval35)
OUT36=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval36)
OUT37=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval37)
OUT38=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval38)
OUT39=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval39)
OUT40=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval40)
OUT41=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval41)
OUT42=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval42)
OUT43=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval43)
OUT44=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval44)
OUT45=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval45)
OUT46=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval46)
OUT47=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval47)
OUT48=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval48)
OUT49=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval49)
OUT50=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval50)
OUT51=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval51)
OUT52=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval52)
OUT53=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval53)
OUT54=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval54)
OUT55=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval55)
OUT56=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval56)
OUT57=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval57)
OUT58=condi4joint(my.bn.par.mn, par = prednames, pour = obsnames, x2 = obsval58)



DFB=data.frame(OUT1$mu,OUT2$mu,OUT3$mu,OUT4$mu,OUT5$mu,OUT6$mu,OUT7$mu,OUT8$mu,OUT9$mu,OUT10$mu,OUT11$mu,OUT12$mu,OUT13$mu,OUT14$mu,OUT15$mu,OUT16$mu,OUT17$mu,OUT18$mu,OUT19$mu,OUT20$mu,OUT21$mu,OUT22$mu,OUT23$mu,OUT24$mu,OUT25$mu,OUT26$mu,OUT27$mu,OUT28$mu,OUT29$mu,OUT30$mu,OUT31$mu,OUT32$mu,OUT33$mu,OUT34$mu,OUT35$mu,OUT36$mu,OUT37$mu,OUT38$mu,OUT39$mu,OUT40$mu,OUT41$mu,OUT42$mu,OUT43$mu,OUT44$mu,OUT45$mu,OUT46$mu,OUT47$mu,OUT48$mu,OUT49$mu,OUT50$mu,OUT51$mu,OUT52$mu,OUT53$mu,OUT54$mu,OUT55$mu,OUT56$mu,OUT57$mu,OUT58$mu)
FRMO <- data.frame(t(DFB))
write.table(FRMO , file = "C:/Users/Kazem Kamrani/Desktop/FRMO.csv",row.names=FALSE , sep=",")


RMSE = function( m, o){
  sqrt(mean((m - o)^2))
}

RMSE(FRMO$R,Baynet$R)       
     

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

mape(FRMO$R,Baynet$R)

plot(FRMO$R,Baynet$R)








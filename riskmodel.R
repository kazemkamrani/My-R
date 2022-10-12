library("nnet")
library("HydeNet")
library("coda")
library("MASS")
library("rjags")

risk <- read.delim("C:/Users/Kazem Kamrani/Desktop/risk.txt")

dag <-HydeNetwork( ~ H2S
                   + BOD5
                   + COD
                   + TSS
                   + FECAL
                   + NO3
                   + PO4
                   + As
                   + Hg
                   + Pb
                   + Cd
                   + HEM
                   + HEM | As*Hg*Pb*Cd
                   + SO4
                   + TH
                   + ALK
                   + SAR
                   + EC
                   + OIL
                   + DET
                   + E1 | H2S
                   + E2 | FECAL
                   + E3 | BOD5*COD*NO3*PO4*HEM*SO4*OIL*DET
                   + E4 | TSS*TH*ALK*SAR*EC
                   + E5 | NO3*HEM
                   + E6 | TSS*ALK*EC*OIL
                   + E7 | E1*DET
                   + E8 | E2*E3*E4
                   + E9 | FECAL
                   + E10 | PO4*SO4*E5*OIL*DET
                   + E11 | TH*SAR*E6
                   + E12 | E8
                   + E13 | E9*E10*E11
                   + E14 | E7*E12*E13
                   + ER | E7*E12*E13*E14
                   + S1 | E8*E14
                   + S2 
                   + S3 
                   + S4 
                   + S5
                   + S6 | S1*S2*S3*S4
                   + S8 | S1*S4
                   + S10 | S8*S5
                   + SR | S6*S10
                   + ECO1
                   + ECO2 | E14*T1
                   + ECO3 | S5*ECO2*T1*T2*T3
                   + ECO4 | S5*ECO2*T2*T3
                   + ECO4 | ECO2*S6*T2*T3*T1
                   + ECO5 | ECO3*ECO4
                   + ECO6 | S6*S5*ECO1*ECO2*T2
                   + ECOR | ECO5*ECO6
                   + T1 | E12
                   + T2
                   + T3
                   + T4 | S5*T2*T1*T3
                   + T5 | S5*T2*T1*T3
                   + T6
                   + TR | T4*T5*T6
                   + R | ER*SR*ECOR*TR,
                   data = risk)

plot(dag) 

HydeNet:::writeJagsModel(dag, node ="H2S")
HydeNet:::writeJagsModel(dag, node ="BOD5")
HydeNet:::writeJagsModel(dag, node ="COD")
HydeNet:::writeJagsModel(dag, node ="TSS")
HydeNet:::writeJagsModel(dag, node ="FECAL")
HydeNet:::writeJagsModel(dag, node ="NO3")
HydeNet:::writeJagsModel(dag, node ="PO4")
HydeNet:::writeJagsModel(dag, node ="As")
HydeNet:::writeJagsModel(dag, node ="Hg")
HydeNet:::writeJagsModel(dag, node ="Pb")
HydeNet:::writeJagsModel(dag, node ="Cd")
HydeNet:::writeJagsModel(dag, node ="HEM")
HydeNet:::writeJagsModel(dag, node ="SO4")
HydeNet:::writeJagsModel(dag, node ="TH")
HydeNet:::writeJagsModel(dag, node ="ALK")
HydeNet:::writeJagsModel(dag, node ="SAR")
HydeNet:::writeJagsModel(dag, node ="EC")
HydeNet:::writeJagsModel(dag, node ="OIL")
HydeNet:::writeJagsModel(dag, node ="DET")
HydeNet:::writeJagsModel(dag, node ="E1")
HydeNet:::writeJagsModel(dag, node ="E7")
HydeNet:::writeJagsModel(dag, node ="E2")
HydeNet:::writeJagsModel(dag, node ="E3")
HydeNet:::writeJagsModel(dag, node ="E4")
HydeNet:::writeJagsModel(dag, node ="E8")
HydeNet:::writeJagsModel(dag, node ="E12")
HydeNet:::writeJagsModel(dag, node ="E5")
HydeNet:::writeJagsModel(dag, node ="E6")
HydeNet:::writeJagsModel(dag, node ="E9")
HydeNet:::writeJagsModel(dag, node ="E10")
HydeNet:::writeJagsModel(dag, node ="E11")
HydeNet:::writeJagsModel(dag, node ="E13")
HydeNet:::writeJagsModel(dag, node ="E14")
HydeNet:::writeJagsModel(dag, node ="ER")
HydeNet:::writeJagsModel(dag, node ="S1")
HydeNet:::writeJagsModel(dag, node ="S2")
HydeNet:::writeJagsModel(dag, node ="S3")
HydeNet:::writeJagsModel(dag, node ="S4")
HydeNet:::writeJagsModel(dag, node ="S5")
HydeNet:::writeJagsModel(dag, node ="S6")
HydeNet:::writeJagsModel(dag, node ="S8")
HydeNet:::writeJagsModel(dag, node ="S10")
HydeNet:::writeJagsModel(dag, node ="SR")
HydeNet:::writeJagsModel(dag, node ="ECO1")
HydeNet:::writeJagsModel(dag, node ="ECO2")
HydeNet:::writeJagsModel(dag, node ="ECO3")
HydeNet:::writeJagsModel(dag, node ="ECO4")
HydeNet:::writeJagsModel(dag, node ="ECO5")
HydeNet:::writeJagsModel(dag, node ="ECO6")
HydeNet:::writeJagsModel(dag, node ="ECOR")
HydeNet:::writeJagsModel(dag, node ="T1")
HydeNet:::writeJagsModel(dag, node ="T2")
HydeNet:::writeJagsModel(dag, node ="T3")
HydeNet:::writeJagsModel(dag, node ="T4")
HydeNet:::writeJagsModel(dag, node ="T5")
HydeNet:::writeJagsModel(dag, node ="T6")
HydeNet:::writeJagsModel(dag, node ="TR")
HydeNet:::writeJagsModel(dag, node ="R")

writeNetworkModel(dag, pretty = TRUE)
 


dag1 <- compileJagsModel(dag)

post1 <- HydePosterior(dag1,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind=FALSE)

plot(post1$codas[,c("R")])

bp1 <- bindPosterior(post1)
head(bp1)
mean(bp1$R)
sd(bp1$R)
summary(bp1$R)


dag2<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=12,COD=38.9,TSS=19.7,FECAL=1,NO3=9.1,PO4=1.1,As=0.1,Hg=0.9,Pb=1.5,Cd=0.1,SO4=110,TH=249,ALK=219.1,SAR=3.4,EC=1.149,OIL=0.3,DET=0.8,S2="L",S3="L",S4="M",S5="M",ECO1="M",T2="LM",T3="L",T6="LM"))
dag3<- compileJagsModel(dag, data= list(H2S=0.8,BOD5=46,COD=159.4,TSS=34.4,FECAL=4.85798099512757,NO3=1.1,PO4=1.3,As=0.1,Hg=0.8,Pb=7.5,Cd=0.3,SO4=130,TH=255,ALK=325.2,SAR=2.9,EC=1.293,OIL=0.5,DET=0.8,S2="L",S3="L",S4="M",S5="M",ECO1="M",T2="LM",T3="L",T6="LM"))
dag4<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=42,COD=68.2,TSS=21.3,FECAL=1,NO3=1.1,PO4=1.4,As=0.01,Hg=0.2,Pb=5.1,Cd=0.3,SO4=130,TH=304,ALK=260.9,SAR=3.1,EC=1.335,OIL=0.5,DET=0.9,S2="L",S3="L",S4="M",S5="M",ECO1="M",T2="LM",T3="L",T6="LM"))
dag5<- compileJagsModel(dag, data= list(H2S=0.2,BOD5=43,COD=109.6,TSS=29.1,FECAL=1,NO3=1.1,PO4=1.4,As=0.01,Hg=0.01,Pb=2.1,Cd=0.01,SO4=136,TH=290,ALK=374.8,SAR=4.5,EC=0.879,OIL=0.4,DET=0.9,S2="L",S3="L",S4="M",S5="M",ECO1="LM",T2="M",T3="L",T6="LM"))
dag6<- compileJagsModel(dag, data= list(H2S=1.3,BOD5=21.8,COD=37.4,TSS=32.3,FECAL=7.78135971352466,NO3=11.7,PO4=3.2,As=1,Hg=0.6,Pb=0.01,Cd=0.01,SO4=125,TH=274,ALK=295,SAR=3.4,EC=1.487,OIL=0.5,DET=0.9,S2="LM",S3="L",S4="M",S5="M",ECO1="M",T2="LM",T3="L",T6="M"))
dag7<- compileJagsModel(dag, data= list(H2S=1.4,BOD5=10,COD=30,TSS=8,FECAL=8.64385618977473,NO3=40,PO4=2.7,As=0.01,Hg=0.01,Pb=0.01,Cd=0.01,SO4=92.2,TH=557,ALK=460,SAR=0.7,EC=0.83,OIL=0.5,DET=2.6,S2="LM",S3="L",S4="LM",S5="HM",ECO1="LM",T2="M",T3="L",T6="LM"))
dag8<- compileJagsModel(dag, data= list(H2S=1,BOD5=60,COD=87,TSS=60,FECAL=10.103287808412,NO3=71,PO4=5.6,As=0.2,Hg=0.7,Pb=50,Cd=10,SO4=447,TH=202,ALK=520,SAR=2.5,EC=2.78,OIL=20,DET=2.5,S2="M",S3="M",S4="L",S5="LM",ECO1="HM",T2="HM",T3="M",T6="HM"))
dag9<- compileJagsModel(dag, data= list(H2S=0.7,BOD5=100,COD=110,TSS=23,FECAL=10.103287808412,NO3=84,PO4=4.8,As=0.2,Hg=3.3,Pb=20,Cd=40,SO4=427,TH=204,ALK=472,SAR=3.6,EC=2.29,OIL=11.5,DET=4.7,S2="M",S3="M",S4="L",S5="LM",ECO1="HM",T2="HM",T3="M",T6="HM"))
dag10<- compileJagsModel(dag, data= list(H2S=0.3,BOD5=90,COD=105,TSS=30,FECAL=10.103287808412,NO3=60,PO4=14,As=4.3,Hg=9.1,Pb=50,Cd=23,SO4=442,TH=214,ALK=690,SAR=3.8,EC=2.62,OIL=22,DET=4.9,S2="M",S3="M",S4="L",S5="LM",ECO1="HM",T2="HM",T3="M",T6="H"))
dag11<- compileJagsModel(dag, data= list(H2S=0.6,BOD5=140,COD=242,TSS=544.9,FECAL=8.84549005094438,NO3=8.6,PO4=2.1,As=15.7,Hg=2.9,Pb=3.1,Cd=1.9,SO4=449,TH=222,ALK=381,SAR=8.3,EC=1.534,OIL=0.3,DET=1,S2="LM",S3="LM",S4="HM",S5="L",ECO1="LM",T2="L",T3="LM",T6="M"))
dag12<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=21,COD=15,TSS=12,FECAL=4.8073549220576,NO3=123,PO4=10,As=2,Hg=0.2,Pb=21,Cd=0.01,SO4=172,TH=834,ALK=400,SAR=0.3,EC=2.214,OIL=1.1,DET=2.1,S2="M",S3="L",S4="LM",S5="HM",ECO1="H",T2="LM",T3="L",T6="LM"))
dag13<- compileJagsModel(dag, data= list(H2S=0.2,BOD5=56,COD=23,TSS=16,FECAL=4.52356195605701,NO3=85,PO4=17,As=2.5,Hg=1,Pb=16,Cd=0.01,SO4=245,TH=537,ALK=320,SAR=0.8,EC=1.776,OIL=0.8,DET=1.7,S2="LM",S3="L",S4="LM",S5="HM",ECO1="HM",T2="L",T3="L",T6="LM"))
dag14<- compileJagsModel(dag, data= list(H2S=0.3,BOD5=32,COD=124,TSS=31,FECAL=5.61470984411521,NO3=106,PO4=2,As=0.6,Hg=0.2,Pb=5,Cd=3.4,SO4=326,TH=771,ALK=380,SAR=0.5,EC=2.113,OIL=2,DET=3,S2="M",S3="L",S4="LM",S5="M",ECO1="H",T2="LM",T3="L",T6="L"))
dag15<- compileJagsModel(dag, data= list(H2S=0.9,BOD5=26,COD=35,TSS=7,FECAL=4.39231742277876,NO3=106,PO4=2,As=3,Hg=0.6,Pb=26,Cd=0.01,SO4=168,TH=672,ALK=410,SAR=0.5,EC=2.014,OIL=2.4,DET=2.2,S2="LM",S3="L",S4="LM",S5="M",ECO1="H",T2="LM",T3="L",T6="L"))
dag16<- compileJagsModel(dag, data= list(H2S=1.4,BOD5=46,COD=87,TSS=18,FECAL=5.28540221886225,NO3=87,PO4=14,As=0.5,Hg=0.1,Pb=19,Cd=1.6,SO4=369,TH=615,ALK=412,SAR=0.9,EC=2.016,OIL=0.9,DET=2.5,S2="LM",S3="L",S4="LM",S5="LM",ECO1="H",T2="LM",T3="L",T6="L"))
dag17<- compileJagsModel(dag, data= list(H2S=0.8,BOD5=22,COD=45,TSS=14,FECAL=5.16992500144231,NO3=114,PO4=2,As=1,Hg=1,Pb=22,Cd=0.01,SO4=246,TH=638,ALK=415,SAR=0.8,EC=2.014,OIL=1.2,DET=1.9,S2="LM",S3="L",S4="LM",S5="M",ECO1="HM",T2="LM",T3="L",T6="L"))
dag18<- compileJagsModel(dag, data= list(H2S=1.2,BOD5=55,COD=43,TSS=23,FECAL=4,NO3=75,PO4=11,As=3.6,Hg=2,Pb=8,Cd=0.01,SO4=348,TH=535,ALK=300,SAR=1,EC=1.796,OIL=0.7,DET=2.2,S2="LM",S3="L",S4="LM",S5="LM",ECO1="HM",T2="LM",T3="L",T6="L"))
dag19<- compileJagsModel(dag, data= list(H2S=1,BOD5=45,COD=87,TSS=54,FECAL=5.16992500144231,NO3=89,PO4=14,As=1.3,Hg=1.2,Pb=17,Cd=3.1,SO4=289,TH=682,ALK=319,SAR=0.7,EC=2.112,OIL=0.9,DET=3.4,S2="LM",S3="L",S4="LM",S5="HM",ECO1="H",T2="L",T3="L",T6="L"))
dag20<- compileJagsModel(dag, data= list(H2S=0.9,BOD5=46,COD=78,TSS=11,FECAL=5.24792751344359,NO3=79,PO4=11,As=2.1,Hg=2,Pb=6,Cd=0.01,SO4=225,TH=630,ALK=300,SAR=0.8,EC=1.883,OIL=2.9,DET=3.8,S2="LM",S3="L",S4="LM",S5="HM",ECO1="HM",T2="L",T3="L",T6="L"))
dag21<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=56,COD=91,TSS=175,FECAL=17.9065383321369,NO3=3.7,PO4=8.5,As=0.1,Hg=1,Pb=0.1,Cd=0.4,SO4=305,TH=349,ALK=181,SAR=4.5,EC=1.383,OIL=3.6,DET=2.6,S2="M",S3="HM",S4="H",S5="H",ECO1="H",T2="HM",T3="HM",T6="HM"))
dag22<- compileJagsModel(dag, data= list(H2S=0.2,BOD5=112,COD=145,TSS=235,FECAL=16.7706896993374,NO3=60.5,PO4=18.4,As=0.01,Hg=0.01,Pb=0.01,Cd=0.01,SO4=391,TH=441,ALK=303.5,SAR=4.2,EC=2.02,OIL=2.6,DET=2,S2="M",S3="HM",S4="H",S5="H",ECO1="H",T2="H",T3="HMM",T6="HM"))
dag23<- compileJagsModel(dag, data= list(H2S=2,BOD5=82.5,COD=118.6,TSS=205,FECAL=18.535639892993,NO3=43.6,PO4=12.2,As=0.01,Hg=0.1,Pb=0.1,Cd=0.01,SO4=424,TH=383,ALK=236.2,SAR=4.4,EC=1.7308,OIL=2,DET=1.6,S2="M",S3="HM",S4="H",S5="H",ECO1="H",T2="HM",T3="HM",T6="H"))
dag24<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=35,COD=77,TSS=68,FECAL=5.90689059560852,NO3=14,PO4=1.7,As=5,Hg=0.1,Pb=0.1,Cd=0.1,SO4=100,TH=471,ALK=584,SAR=7.6,EC=2.505,OIL=7.4,DET=2,S2="L",S3="M",S4="H",S5="H",ECO1="M",T2="H",T3="L",T6="M"))
dag25<- compileJagsModel(dag, data= list(H2S=0.5,BOD5=69.1,COD=139.5,TSS=89.3,FECAL=8.97154355395077,NO3=27.4,PO4=3.2,As=0.01,Hg=0.01,Pb=0.8,Cd=0.01,SO4=416,TH=167,ALK=292.8,SAR=2.8,EC=2.583,OIL=8.4,DET=1.5,S2="L",S3="L",S4="M",S5="LM",ECO1="LM",T2="L",T3="LM",T6="HM"))
dag26<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=45,COD=100,TSS=14,FECAL=4.16992500144231,NO3=3,PO4=3.3,As=0.01,Hg=1.9,Pb=5.1,Cd=0.01,SO4=491,TH=402,ALK=280.2,SAR=4.8,EC=1.11,OIL=0.5,DET=0.6,S2="HM",S3="M",S4="HM",S5="HM",ECO1="M",T2="M",T3="HM",T6="H"))
dag27<- compileJagsModel(dag, data= list(H2S=1.6,BOD5=246,COD=436,TSS=45,FECAL=10.9657842846621,NO3=22,PO4=2.1,As=0.5,Hg=0.5,Pb=16,Cd=1.2,SO4=521,TH=216,ALK=349.4,SAR=6.6,EC=0.506,OIL=1.2,DET=0.1,S2="M",S3="HM",S4="M",S5="HM",ECO1="HM",T2="M",T3="M",T6="H"))
dag28<- compileJagsModel(dag, data= list(H2S=0.7,BOD5=48,COD=108,TSS=53,FECAL=8.67948009950545,NO3=3.8,PO4=3.7,As=6.1,Hg=1.5,Pb=1.1,Cd=1,SO4=522,TH=353,ALK=270.2,SAR=0.9,EC=1.006,OIL=1.6,DET=1.2,S2="LM",S3="M",S4="HM",S5="M",ECO1="HM",T2="M",T3="HM",T6="H"))
dag29<- compileJagsModel(dag, data= list(H2S=0.01,BOD5=41,COD=87,TSS=9,FECAL=3.32192809488736,NO3=8,PO4=7.7,As=11.7,Hg=4.1,Pb=2.1,Cd=2.9,SO4=487,TH=352,ALK=385.2,SAR=5,EC=1.807,OIL=1.9,DET=1.4,S2="L",S3="L",S4="LM",S5="M",ECO1="M",T2="LM",T3="L",T6="LM"))
dag30<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=28,COD=60.7,TSS=21,FECAL=5.58496250072116,NO3=2,PO4=3,As=1,Hg=0.01,Pb=0.01,Cd=0.01,SO4=503,TH=310,ALK=325.2,SAR=4.5,EC=1.5297,OIL=5,DET=1,S2="HM",S3="M",S4="H",S5="M",ECO1="M",T2="M",T3="M",T6="HM"))
dag31<- compileJagsModel(dag, data= list(H2S=2,BOD5=10,COD=19.1,TSS=29,FECAL=7.71424551766612,NO3=1.6,PO4=0.8,As=0.1,Hg=0.01,Pb=1.2,Cd=0.01,SO4=499,TH=334,ALK=420.2,SAR=2.3,EC=1.78,OIL=16,DET=3.1,S2="M",S3="HM",S4="M",S5="M",ECO1="M",T2="M",T3="HM",T6="HM"))
dag32<- compileJagsModel(dag, data= list(H2S=1.1,BOD5=29,COD=62,TSS=99,FECAL=6.06608919045777,NO3=2,PO4=3.7,As=9.8,Hg=3.2,Pb=1.7,Cd=1.9,SO4=532,TH=281,ALK=385.2,SAR=6.2,EC=1.879,OIL=3.1,DET=1.3,S2="LM",S3="M",S4="HM",S5="M",ECO1="M",T2="LM",T3="L",T6="HM"))
dag33<- compileJagsModel(dag, data= list(H2S=0.01,BOD5=8.1,COD=14,TSS=18,FECAL=3.90689059560852,NO3=3.8,PO4=18,As=6,Hg=3,Pb=0.01,Cd=0.01,SO4=171.5,TH=475,ALK=300.2,SAR=2.7,EC=1.717,OIL=4,DET=1.2,S2="L",S3="L",S4="M",S5="LM",ECO1="L",T2="L",T3="L",T6="L"))
dag34<- compileJagsModel(dag, data= list(H2S=0.1,BOD5=16.8,COD=32,TSS=15,FECAL=2.58496250072116,NO3=11,PO4=20,As=7,Hg=3,Pb=0.01,Cd=0.01,SO4=182.1,TH=360,ALK=310.1,SAR=2.9,EC=1.722,OIL=3.9,DET=1.9,S2="L",S3="L",S4="HM",S5="LM",ECO1="L",T2="L",T3="L",T6="LM"))
dag35<- compileJagsModel(dag, data= list(H2S=0.4,BOD5=8.1,COD=14,TSS=15,FECAL=2.58496250072116,NO3=3.8,PO4=34,As=6,Hg=0.3,Pb=0.01,Cd=0.01,SO4=171.5,TH=360,ALK=300.2,SAR=2.7,EC=1.717,OIL=2.9,DET=2,S2="L",S3="L",S4="LM",S5="LM",ECO1="L",T2="L",T3="L",T6="M"))
dag36<- compileJagsModel(dag, data= list(H2S=2.5,BOD5=107,COD=203,TSS=115,FECAL=10.9549232920303,NO3=46,PO4=54,As=3.4,Hg=3.5,Pb=0.01,Cd=3,SO4=265.7,TH=699,ALK=445.1,SAR=5.2,EC=2.351,OIL=1.8,DET=1.8,S2="M",S3="L",S4="LM",S5="LM",ECO1="LM",T2="L",T3="L",T6="M"))
dag37<- compileJagsModel(dag, data= list(H2S=2.3,BOD5=88,COD=200,TSS=163,FECAL=10.2526654324503,NO3=149.9,PO4=8,As=2.1,Hg=1,Pb=0.01,Cd=3.8,SO4=444.4,TH=864,ALK=425.1,SAR=5.1,EC=2.34,OIL=2,DET=2,S2="LM",S3="L",S4="M",S5="L",ECO1="M",T2="L",T3="L",T6="M"))
dag38<- compileJagsModel(dag, data= list(H2S=1.8,BOD5=107,COD=203,TSS=163,FECAL=10.9549232920303,NO3=149.9,PO4=7,As=3.4,Hg=3.5,Pb=0.1,Cd=3.8,SO4=444.4,TH=864,ALK=445.1,SAR=5.2,EC=2.351,OIL=2.1,DET=0.9,S2="M",S3="L",S4="M",S5="L",ECO1="M",T2="L",T3="L",T6="M"))
dag39<- compileJagsModel(dag, data= list(H2S=2.3,BOD5=52.5,COD=103.8,TSS=61.2,FECAL=9.75488750216347,NO3=24.1,PO4=37,As=2.1,Hg=0.8,Pb=0.1,Cd=0.4,SO4=224.1,TH=582,ALK=362,SAR=3.4,EC=1.904,OIL=1.1,DET=1.1,S2="LM",S3="L",S4="M",S5="LM",ECO1="LM",T2="L",T3="L",T6="M"))
dag40<- compileJagsModel(dag, data= list(H2S=0.9,BOD5=49.4,COD=104.7,TSS=69.6,FECAL=8.56224242422107,NO3=57.7,PO4=28,As=1.3,Hg=0.8,Pb=0.2,Cd=0.8,SO4=256.5,TH=567,ALK=348,SAR=3.6,EC=1.9281,OIL=0.9,DET=1.7,S2="LM",S3="L",S4="M",S5="LM",ECO1="LM",T2="L",T3="L",T6="M"))
dag41<- compileJagsModel(dag, data= list(H2S=1.3,BOD5=51.3,COD=104.2,TSS=64.4,FECAL=9.39874369193819,NO3=37.2,PO4=13,As=1.8,Hg=0.8,Pb=0.01,Cd=0.6,SO4=240.3,TH=576,ALK=356.5,SAR=3.5,EC=1.9134,OIL=0.8,DET=2.1,S2="LM",S3="L",S4="M",S5="LM",ECO1="LM",T2="L",T3="L",T6="M"))
dag42<- compileJagsModel(dag, data= list(H2S=3,BOD5=83,COD=236,TSS=111,FECAL=9.96578428466209,NO3=9,PO4=22.9,As=2,Hg=6.1,Pb=60,Cd=1.1,SO4=310,TH=1290,ALK=172.3,SAR=2.3,EC=1.42,OIL=2,DET=1.5,S2="LM",S3="LM",S4="M",S5="L",ECO1="L",T2="L",T3="L",T6="M"))
dag43<- compileJagsModel(dag, data= list(H2S=0.01,BOD5=9,COD=8,TSS=7,FECAL=0,NO3=3.6,PO4=0.5,As=0.01,Hg=0.01,Pb=0.01,Cd=0.01,SO4=160,TH=214,ALK=96.4,SAR=3.1,EC=0.84,OIL=0.1,DET=0.9,S2="L",S3="L",S4="L",S5="LM",ECO1="LM",T2="H",T3="L",T6="L"))
dag44<- compileJagsModel(dag, data= list(H2S=2.8,BOD5=32.1,COD=35,TSS=25.2,FECAL=14.3581017074408,NO3=3.4,PO4=8.2,As=10,Hg=10,Pb=80,Cd=20,SO4=192,TH=161,ALK=257,SAR=3.5,EC=0.38,OIL=1.4,DET=1.9,S2="LM",S3="L",S4="L",S5="H",ECO1="M",T2="HM",T3="L",T6="L"))
dag45<- compileJagsModel(dag, data= list(H2S=0.01,BOD5=5,COD=17.5,TSS=11,FECAL=1,NO3=188.3,PO4=41.4,As=0.01,Hg=0.01,Pb=0.01,Cd=0.01,SO4=200,TH=150,ALK=159,SAR=1,EC=4.074,OIL=3.2,DET=0.7,S2="L",S3="L",S4="L",S5="LM",ECO1="L",T2="H",T3="M",T6="M"))
dag46<- compileJagsModel(dag, data= list(H2S=5,BOD5=23,COD=40,TSS=13,FECAL=13.4252159032994,NO3=2.1,PO4=0.5,As=0.01,Hg=2,Pb=50,Cd=10,SO4=510,TH=2940,ALK=152.2,SAR=5.5,EC=5.05,OIL=8.8,DET=0.6,S2="H",S3="M",S4="HM",S5="L",ECO1="LM",T2="LM",T3="M",T6="HM"))


post2 <- HydePosterior(dag2,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)

post2 <- HydePosterior(dag2,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post3<- HydePosterior(dag3,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post4<- HydePosterior(dag4,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post5<- HydePosterior(dag5,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post6<- HydePosterior(dag6,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post7<- HydePosterior(dag7,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post8<- HydePosterior(dag8,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post9<- HydePosterior(dag9,
                      variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                      n.iter = 10000,
                      bind = FALSE)
post10<- HydePosterior(dag10,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post11<- HydePosterior(dag11,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post12<- HydePosterior(dag12,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post13<- HydePosterior(dag13,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post14<- HydePosterior(dag14,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post15<- HydePosterior(dag15,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post16<- HydePosterior(dag16,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post17<- HydePosterior(dag17,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post18<- HydePosterior(dag18,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post19<- HydePosterior(dag19,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post20<- HydePosterior(dag20,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post21<- HydePosterior(dag21,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post22<- HydePosterior(dag22,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post23<- HydePosterior(dag23,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post24<- HydePosterior(dag24,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post25<- HydePosterior(dag25,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post26<- HydePosterior(dag26,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post27<- HydePosterior(dag27,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post28<- HydePosterior(dag28,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post29<- HydePosterior(dag29,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post30<- HydePosterior(dag30,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post31<- HydePosterior(dag31,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post32<- HydePosterior(dag32,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post33<- HydePosterior(dag33,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post34<- HydePosterior(dag34,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post35<- HydePosterior(dag35,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post36<- HydePosterior(dag36,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post37<- HydePosterior(dag37,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post38<- HydePosterior(dag38,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post39<- HydePosterior(dag39,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post40<- HydePosterior(dag40,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post41<- HydePosterior(dag41,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post42<- HydePosterior(dag42,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post43<- HydePosterior(dag43,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post44<- HydePosterior(dag44,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post45<- HydePosterior(dag45,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)
post46<- HydePosterior(dag46,
                       variable.names = c("H2S",	"BOD5",	"COD",	"TSS",	"FECAL",	"NO3",	"PO4",	"As",	"Hg",	"Pb",	"Cd",	"HEM",	"SO4",	"TH",	"ALK",	"SAR",	"EC",	"OIL",	"DET",	"E1",	"E7",	"E2",	"E3",	"E4",	"E8",	"E12",	"E5",	"E6",	"E9",	"E10",	"E11",	"E13",	"E14",	"ER",	"S1",	"S2",	"S3",	"S4",	"S5",	"S6",	"S7",	"S8",	"S9",	"S10",	"SR",	"ECO1",	"ECO2",	"ECO3",	"ECO4",	"ECO5",	"ECO6",	"ECOR",	"T1",	"T2",	"T3",	"T4",	"T5",	"T6",	"TR",	"R"),
                       n.iter = 10000,
                       bind = FALSE)




bp2 <- bindPosterior(post2)
bp3 <- bindPosterior(post3)
bp4 <- bindPosterior(post4)
bp5 <- bindPosterior(post5)
bp6 <- bindPosterior(post6)
bp7 <- bindPosterior(post7)
bp8 <- bindPosterior(post8)
bp9 <- bindPosterior(post9)
bp10 <- bindPosterior(post10)
bp11 <- bindPosterior(post11)
bp12 <- bindPosterior(post12)
bp13 <- bindPosterior(post13)
bp14 <- bindPosterior(post14)
bp15 <- bindPosterior(post15)
bp16 <- bindPosterior(post16)
bp17 <- bindPosterior(post17)
bp18 <- bindPosterior(post18)
bp19 <- bindPosterior(post19)
bp20 <- bindPosterior(post20)
bp21 <- bindPosterior(post21)
bp22 <- bindPosterior(post22)
bp23 <- bindPosterior(post23)
bp24 <- bindPosterior(post24)
bp25 <- bindPosterior(post25)
bp26 <- bindPosterior(post26)
bp27 <- bindPosterior(post27)
bp28 <- bindPosterior(post28)
bp29 <- bindPosterior(post29)
bp30 <- bindPosterior(post30)
bp31 <- bindPosterior(post31)
bp32 <- bindPosterior(post32)
bp33 <- bindPosterior(post33)
bp34 <- bindPosterior(post34)
bp35 <- bindPosterior(post35)
bp36 <- bindPosterior(post36)
bp37 <- bindPosterior(post37)
bp38 <- bindPosterior(post38)
bp39 <- bindPosterior(post39)
bp40 <- bindPosterior(post40)
bp41 <- bindPosterior(post41)
bp42 <- bindPosterior(post42)
bp43 <- bindPosterior(post43)
bp44 <- bindPosterior(post44)
bp45 <- bindPosterior(post45)
bp46 <- bindPosterior(post46)

plot(post2$codas[,c("R")])
head(bp2)
mean(bp2$R)

MEAN2=mean(bp2$R)
MEAN3=mean(bp3$R)
MEAN4=mean(bp4$R)
MEAN5=mean(bp5$R)
MEAN6=mean(bp6$R)
MEAN7=mean(bp7$R)
MEAN8=mean(bp8$R)
MEAN9=mean(bp9$R)
MEAN10=mean(bp10$R)
MEAN11=mean(bp11$R)
MEAN12=mean(bp12$R)
MEAN13=mean(bp13$R)
MEAN14=mean(bp14$R)
MEAN15=mean(bp15$R)
MEAN16=mean(bp16$R)
MEAN17=mean(bp17$R)
MEAN18=mean(bp18$R)
MEAN19=mean(bp19$R)
MEAN20=mean(bp20$R)
MEAN21=mean(bp21$R)
MEAN22=mean(bp22$R)
MEAN23=mean(bp23$R)
MEAN24=mean(bp24$R)
MEAN25=mean(bp25$R)
MEAN26=mean(bp26$R)
MEAN27=mean(bp27$R)
MEAN28=mean(bp28$R)
MEAN29=mean(bp29$R)
MEAN30=mean(bp30$R)
MEAN31=mean(bp31$R)
MEAN32=mean(bp32$R)
MEAN33=mean(bp33$R)
MEAN34=mean(bp34$R)
MEAN35=mean(bp35$R)
MEAN36=mean(bp36$R)
MEAN37=mean(bp37$R)
MEAN38=mean(bp38$R)
MEAN39=mean(bp39$R)
MEAN40=mean(bp40$R)
MEAN41=mean(bp41$R)
MEAN42=mean(bp42$R)
MEAN43=mean(bp43$R)
MEAN44=mean(bp44$R)
MEAN45=mean(bp45$R)
MEAN46=mean(bp46$R)


MODELDATA=data.frame(MEAN2,	MEAN3,	MEAN4,	MEAN5,	MEAN6,	MEAN7,	MEAN8,	MEAN9,	MEAN10,	MEAN11,	MEAN12,	MEAN13,	MEAN14,	MEAN15,	MEAN16,	MEAN17,	MEAN18,	MEAN19,	MEAN20,	MEAN21,	MEAN22,	MEAN23,	MEAN24,	MEAN25,	MEAN26,	MEAN27,	MEAN28,	MEAN29,	MEAN30,	MEAN31,	MEAN32,	MEAN33,	MEAN34,	MEAN35,	MEAN36,	MEAN37,	MEAN38,	MEAN39,	MEAN40,	MEAN41,	MEAN42,	MEAN43,	MEAN44,	MEAN45,	MEAN46)
DATAMODEL <- data.frame(t(MODELDATA))

RMSE = function( m, o){
  sqrt(mean((m - o)^2))
}

RMSE(DATAMODEL$t.MODELDATA.,risk$R)       


mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


mape(DATAMODEL$t.MODELDATA.,risk$R)

plot(DATAMODEL$t.MODELDATA.,risk$R)











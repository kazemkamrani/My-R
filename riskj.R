Riskdata <- read.delim("C:/Users/Kazem Kamrani/Desktop/Bayesian Networks with Examples in R/Javad/Riskdata.txt")
library("nnet")
library("HydeNet")




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
                   + S7 | S1*S2*S3*S4
                   + S8 | S1*S4
                   + S9 | S6*S7
                   + S10 | S7*S8*S5
                   + SR | S9*S10
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
                   data = Riskdata)
                   
plot(dag) 

library("coda")
library("MASS")
library("rjags")

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
HydeNet:::writeJagsModel(dag, node ="S7")
HydeNet:::writeJagsModel(dag, node ="S8")
HydeNet:::writeJagsModel(dag, node ="S9")
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







library("nnet")
library("HydeNet")
Riskdata <- read.delim("C:/Users/Kazem Kamrani/Desktop/Bayesian Networks with Examples in R/Javad/Riskdata.txt")

Rnet <- HydeNetwork(~ H2S
                    + BOD5
                    + COD
                    + TSS
                    + FECAL
                    + NO3
                    + PO4
                    + HEM
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
                    + S9 | S6
                    + S10 | S7*S8*S5
                    + SR | S9*S10
                    + ECO1
                    + ECO2 | E14*T1
                    + ECO3 | S5*ECO2*T1*T2*T3
                    + ECO4 | S5*ECO2*T2*T3
                    + ECO4 | ECO2*S6*T2*T3*T1
                    + ECO5 | ECO3
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
plot(Rnet)



library("rjags")

HydeNet:::writeJagsModel(Rnet, node ="H2S")
HydeNet:::writeJagsModel(Rnet, node ="BOD5")
HydeNet:::writeJagsModel(Rnet, node ="COD")
HydeNet:::writeJagsModel(Rnet, node ="TSS")
HydeNet:::writeJagsModel(Rnet, node ="FECAL")
HydeNet:::writeJagsModel(Rnet, node ="NO3")
HydeNet:::writeJagsModel(Rnet, node ="PO4")
HydeNet:::writeJagsModel(Rnet, node ="HEM")
HydeNet:::writeJagsModel(Rnet, node ="SO4")
HydeNet:::writeJagsModel(Rnet, node ="TH")
HydeNet:::writeJagsModel(Rnet, node ="ALK")
HydeNet:::writeJagsModel(Rnet, node ="SAR")
HydeNet:::writeJagsModel(Rnet, node ="EC")
HydeNet:::writeJagsModel(Rnet, node ="OIL")
HydeNet:::writeJagsModel(Rnet, node ="DET")
HydeNet:::writeJagsModel(Rnet, node ="E1")
HydeNet:::writeJagsModel(Rnet, node ="E7")
HydeNet:::writeJagsModel(Rnet, node ="E2")
HydeNet:::writeJagsModel(Rnet, node ="E3")
HydeNet:::writeJagsModel(Rnet, node ="E4")
HydeNet:::writeJagsModel(Rnet, node ="E8")
HydeNet:::writeJagsModel(Rnet, node ="E12")
HydeNet:::writeJagsModel(Rnet, node ="E5")
HydeNet:::writeJagsModel(Rnet, node ="E6")
HydeNet:::writeJagsModel(Rnet, node ="E9")
HydeNet:::writeJagsModel(Rnet, node ="E10")
HydeNet:::writeJagsModel(Rnet, node ="E11")
HydeNet:::writeJagsModel(Rnet, node ="E13")
HydeNet:::writeJagsModel(Rnet, node ="E14")
HydeNet:::writeJagsModel(Rnet, node ="ER")
HydeNet:::writeJagsModel(Rnet, node ="S1")
HydeNet:::writeJagsModel(Rnet, node ="S2")
HydeNet:::writeJagsModel(Rnet, node ="S3")
HydeNet:::writeJagsModel(Rnet, node ="S4")
HydeNet:::writeJagsModel(Rnet, node ="S5")
HydeNet:::writeJagsModel(Rnet, node ="S6")
HydeNet:::writeJagsModel(Rnet, node ="S7")
HydeNet:::writeJagsModel(Rnet, node ="S8")
HydeNet:::writeJagsModel(Rnet, node ="S9")
HydeNet:::writeJagsModel(Rnet, node ="S10")
HydeNet:::writeJagsModel(Rnet, node ="SR")
HydeNet:::writeJagsModel(Rnet, node ="ECO1")
HydeNet:::writeJagsModel(Rnet, node ="ECO2")
HydeNet:::writeJagsModel(Rnet, node ="ECO3")
HydeNet:::writeJagsModel(Rnet, node ="ECO4")
HydeNet:::writeJagsModel(Rnet, node ="ECO5")
HydeNet:::writeJagsModel(Rnet, node ="ECO6")
HydeNet:::writeJagsModel(Rnet, node ="ECOR")
HydeNet:::writeJagsModel(Rnet, node ="T1")
HydeNet:::writeJagsModel(Rnet, node ="T2")
HydeNet:::writeJagsModel(Rnet, node ="T3")
HydeNet:::writeJagsModel(Rnet, node ="T4")
HydeNet:::writeJagsModel(Rnet, node ="T5")
HydeNet:::writeJagsModel(Rnet, node ="T6")
HydeNet:::writeJagsModel(Rnet, node ="TR")
HydeNet:::writeJagsModel(Rnet, node ="R")
writeNetworkModel(Rnet, pretty = TRUE)
Rnet1 <- compileJagsModel(Rnet)

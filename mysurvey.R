library(bnlearn)
##Grapgical Representation
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set

#Estimating the Parameters:Conditional Probability Tables
survey <- read.csv("C:/Users/Kazem Kamrani/Desktop/Bayesian Networks with Examples in R/survey.txt", sep="")

bn.mle <- bn.fit(dag , data = survey , method = "mle")
bn.mle

library(gRbase)
library(gRain)

junction <- compile(as.grain(bn.mle))

options(digits = 4)

querygrain(junction, nodes = "T")$T
jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")$T

jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T
jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S", "T"),
                      type = "joint")
SxT.cpt
querygrain(jedu, nodes = c("S", "T"), type = "marginal")
querygrain(jedu, nodes = c("S", "T"), type = "conditional")
dsep(bn.mle, x = "S", y = "T", z = "E")
SxT.ct = SxT.cpt * nrow(survey)

chisq.test(SxT.ct)

set.seed(123)
cpquery(bn.mle, event = (S == "M") & (T == "car"), 
        evidence = (E == "high"))
cpquery(bn.mle, event = (S == "M") & (T == "car"), 
        evidence = (E == "high"), n = 10^6)
set.seed(567)
SxT <- cpdist(bn.mle, nodes = c("S", "T"),
              evidence = (E == "high"))
head(SxT)
graphviz.plot(dag)





                    









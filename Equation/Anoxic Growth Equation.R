# Anoxic growth rate equation
anoxic_growth <- function(time, state, parms) {
  XH <- state[1]
  SO <- state[2]
  SNO <- state[3]
  SNH <- state[4]
  SALK <- state[5]
  XSTO <- state[6]
  
  ??H = parms["??H"]
  KO2 = parms["KO2"]
  KNOX = parms["KNOX"]
  KNH4 = parms["KNH4"]
  KALK = parms["KALK"] 
  KSTO = parms["KSTO"]
  etaox = parms["etaox"]  # Replaced ??ox with etaox
  
  rate = ??H * etaox * (KO2 / (KO2 + SO)) * (SNO / (KNOX + SNO)) * (SNH / (KNH4 + SNH)) *
    (SALK / (KALK + SALK)) * (XSTO / XH) * (KSTO + XSTO / XH) * XH
  
  dXH <- rate
  dSO <- 0  # No change in SO for this example
  dSNO <- 0  # No change in SNO for this example
  dSNH <- 0  # No change in SNH for this example
  dSALK <- 0  # No change in SALK for this example
  dXSTO <- 0  # No change in XSTO for this example
  
  return(list(c(dXH, dSO, dSNO, dSNH, dSALK, dXSTO)))
}

# Parameters
params <- c(??H = 2, KO2 = 0.2, KNOX = 0.5, KNH4 = 0.01, KALK = 0.1, 
            KSTO = 1, etaox = 0.6)  # Replaced ??ox with etaox

# Initial conditions
initial_conditions <- c(XH = 30, SO = 0.5, SNO = 10, SNH = 20, SALK = 5, XSTO = 15)

# Time steps 
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t,
           func = anoxic_growth, parms = params)

# Plot results
matplot(out[, 1:6], type = "l", xlab = "Time (days)")
legend("topright", legend = c("XH", "SO", "SNO", "SNH", "SALK", "XSTO"), 
       lty = 1, col = 1:6, cex = 0.8)

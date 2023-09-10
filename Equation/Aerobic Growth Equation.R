# Aerobic growth rate equation
aerobic_growth <- function(time, state, parms) {
  XH <- state[1]
  SO <- state[2]
  SNH <- state[3]
  SALK <- state[4]
  XSTO <- state[5]
  
  muH = parms["muH"]  # Corrected variable name
  KO2 = parms["KO2"]
  KNH4 = parms["KNH4"] 
  KALK = parms["KALK"]
  KSTO = parms["KSTO"]
  
  rate = muH * (SO / (KO2 + SO)) * (SNH / (KNH4 + SNH)) * (SALK / (KALK + SALK)) *
    (XSTO / XH) * (KSTO + XSTO / XH) * XH
  
  dXH <- rate
  dSO <- 0  # No change in SO for this example
  dSNH <- 0  # No change in SNH for this example
  dSALK <- 0  # No change in SALK for this example
  dXSTO <- 0  # No change in XSTO for this example
  
  return(list(c(dXH, dSO, dSNH, dSALK, dXSTO)))
}

# Parameters
params <- c(muH = 2, KO2 = 0.2, KNH4 = 0.01, KALK = 0.1, KSTO = 1)

# Initial conditions 
initial_conditions <- c(XH = 30, SO = 2, SNH = 20, SALK = 5, XSTO = 10)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, 
           func = aerobic_growth, parms = params)

# Plot results
matplot(out[, 1:5], type = "l", xlab = "Time (days)")
legend("topright", legend = c("XH", "SO", "SNH", "SALK", "XSTO"),
       lty = 1, col = 1:5, cex = 0.8)

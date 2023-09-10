# Anoxic endogenous respiration of autotrophs rate equation
anoxic_auto_endo_resp <- function(time, state, parms) {
  XA <- state[1]
  SO <- state[2]
  SNO <- state[3]
  
  bA_NOX = parms["bA_NOX"]
  KO2 = parms["KO2"]
  KNOX = parms["KNOX"]
  
  rate = bA_NOX * (KO2 / (KO2 + SO)) * (SNO / (KNOX + SNO)) * XA
  
  dXA <- -rate  # Negative value as it's a consumption
  dSO <- 0  # No change in SO for this example
  dSNO <- 0  # No change in SNO for this example
  
  return(list(c(dXA, dSO, dSNO)))
}

# Parameters
params <- c(bA_NOX = 0.05, KO2 = 0.2, KNOX = 0.5)

# Initial conditions
initial_conditions <- c(XA = 10, SO = 0.5, SNO = 10)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, 
           func = anoxic_auto_endo_resp, parms = params)

# Plot results
matplot(out[, 1:3], type = "l", xlab = "Time (days)")
legend("topright", legend = c("XA", "SO", "SNO"), lty = 1, col = 1:3)


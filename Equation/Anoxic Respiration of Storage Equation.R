# Anoxic respiration of storage rate equation
anoxic_resp_storage <- function(time, state, parms) {
  XSTO <- state[1]
  SO <- state[2]
  SNO <- state[3]
  
  bSTO_NOX = parms["bSTO_NOX"]
  KO2 = parms["KO2"]
  KNOX = parms["KNOX"]
  
  rate = bSTO_NOX * (KO2 / (KO2 + SO)) * (SNO / (KNOX + SNO)) * XSTO
  
  dXSTO <- -rate  # Negative value as it's a consumption
  dSO <- 0  # No change in SO for this example
  dSNO <- 0  # No change in SNO for this example
  
  return(list(c(dXSTO, dSO, dSNO)))
}

# Parameters
params <- c(bSTO_NOX = 0.1, KO2 = 0.2, KNOX = 0.5)

# Initial conditions
initial_conditions <- c(XSTO = 15, SO = 0.5, SNO = 10)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, 
           func = anoxic_resp_storage, parms = params)

# Plot results
matplot(out[, 1:3], type = "l", xlab = "Time (days)")
legend("topright", legend = c("XSTO", "SO", "SNO"), lty = 1, col = 1:3)

# Aerobic respiration of storage rate equation
aerobic_resp_storage <- function(time, state, parms) {
  XSTO <- state[1]
  SO <- state[2]
  
  bSTO_O2 = parms["bSTO_O2"]
  KO2 = parms["KO2"]
  
  rate = bSTO_O2 * (SO / (KO2 + SO)) * XSTO
  
  dXSTO <- -rate  # Negative value as it's a consumption
  dSO <- 0  # No change in SO for this example
  
  return(list(c(dXSTO, dSO)))
}

# Parameters
params <- c(bSTO_O2 = 0.2, KO2 = 0.2)

# Initial conditions
initial_conditions <- c(XSTO = 15, SO = 2)

# Time steps
t <- seq(0, 5, 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, func = aerobic_resp_storage,
           parms = params)

# Plot results
plot(out[, 1], out[, 2], type = "l", 
     xlab = "Time (days)", ylab = "Concentration (g COD/m3)")
legend("topright", legend = c("XSTO", "SO"), lty = 1, col = 1:2)


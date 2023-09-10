# Aerobic endogenous respiration rate equation
endo_resp <- function(time, state, parms) {
  XH <- state[1]
  SO <- state[2]
  
  bH_O2 = parms["bH_O2"]
  KO2 = parms["KO2"]
  
  rate = bH_O2 * (SO / (KO2 + SO)) * XH
  
  dXH <- -rate  # Negative value as it's a consumption
  dSO <- 0  # No change in SO for this example
  
  return(list(c(dXH, dSO)))
}

# Parameters
params <- c(bH_O2 = 0.2, KO2 = 0.2)

# Initial conditions
initial_conditions <- c(XH = 30, SO = 2)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, func = endo_resp, parms = params)

# Plot results
plot(out[, 1], out[, 2], type = "l", 
     xlab = "Time (days)", ylab = "Concentration (g COD/m3)")
legend("topright", legend = c("XH", "SO"), lty = 1, col = 1:2)


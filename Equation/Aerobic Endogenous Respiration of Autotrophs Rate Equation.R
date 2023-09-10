# Aerobic endogenous respiration of autotrophs rate equation
auto_endo_resp <- function(time, state, parms) {
  XA <- state[1]
  SO <- state[2]
  
  bA_O2 = parms["bA_O2"]
  KO2 = parms["KO2"]
  
  rate = bA_O2 * (SO / (KO2 + SO)) * XA
  
  dXA <- -rate  # Negative value as it's a consumption
  dSO <- 0  # No change in SO for this example
  
  return(list(c(dXA, dSO)))
}

# Parameters
params <- c(bA_O2 = 0.15, KO2 = 0.2)

# Initial conditions
initial_conditions <- c(XA = 10, SO = 2)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration
library(deSolve)
out <- ode(y = initial_conditions, times = t, func = auto_endo_resp, 
           parms = params)

# Plot results
plot(out[, 1], out[, 2], type = "l",
     xlab = "Time (days)", ylab = "Concentration (g COD/m3)") 
legend("topright", legend = c("XA", "SO"), lty = 1, col = 1:2)


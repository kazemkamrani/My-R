# Calculate the anoxic storage rate
calculate_anoxic_storage_rate <- function(params, XH, SS, SO, SNO) {
  kSTO = params["kSTO"]
  KS = params["KS"]
  KO2 = params["KO2"] 
  KNOX = params["KNOX"]
  etaox = params["etaox"]
  
  rate = kSTO * etaox * (KO2 / (KO2 + SO)) * (SNO / (KNOX + SNO)) * (SS / (KS + SS)) * XH
  
  return(rate)
}

# Parameters
params <- c(kSTO = 10, KS = 10, KO2 = 0.5, KNOX = 0.5, etaox = 0.6)

# Function that wraps the calculation for use with 'ode'
anoxic_storage_wrapper <- function(t, state, params) {
  XH <- state[1]
  SS <- state[2]
  SO <- state[3]
  SNO <- state[4]
  
  rate <- calculate_anoxic_storage_rate(params, XH, SS, SO, SNO)
  
  dXH <- rate
  dSS <- 0  # No change in SS for this example
  dSO <- 0  # No change in SO for this example
  dSNO <- 0  # No change in SNO for this example
  
  derivatives <- c(dXH, dSS, dSO, dSNO)
  
  return(list(derivatives))  # Return derivatives as a list
}

# Initial conditions
initial_conditions <- c(XH = 30, SS = 50, SO = 0.5, SNO = 10)

# Time steps
t <- seq(0, 5, by = 0.05)

# Numerical integration 
library(deSolve)
out <- ode(y = initial_conditions, times = t, func = anoxic_storage_wrapper, parms = params)

# Results
plot(out[, 1], out[, 2], type = "l", 
     xlab = "Time (days)", ylab = "Concentration (g COD/m3)")
lines(out[, 3:4], col = "blue")
legend("topright", legend = c("XH", "SS", "SO", "SNO"), lty = 1, col = 1:4)

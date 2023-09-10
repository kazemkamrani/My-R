# Define hydrolysis rate equation
hydrolysis <- function(t, state, params) {
  XH <- state[1]
  XS <- state[2]
  
  kH <- params["kH"]
  KX <- params["KX"]
  
  # Equations for rates
  rate1 = -kH * (XS/XH) * (KX + XS/XH) * XH
  rate2 = kH * (XS/XH) * (KX + XS/XH) * XH
  
  # Return list of derivatives
  return(list(c(rate1, rate2)))
}

# Parameters
params <- c(kH = 3, KX = 1)

# Initial conditions
state <- c(XH = 30, XS = 100) # Initial state vector

# Time steps
t <- seq(0, 10, by=0.1)

# Numerical integration
library(deSolve)
out <- ode(y = state, times = t, func = hydrolysis, parms = params)

# Results
plot(out[,1], out[,2], type="l",
     xlab="Time (days)", ylab="Concentration (g COD/m3)")
legend("topright", legend=c("XH", "XS"), lty=1, col=1:2)


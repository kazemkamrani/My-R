# Define aerobic storage rate equation
aero_storage <- function(t, state, params) {
  XH <- state[1]
  SS <- state[2]
  SO <- state[3]
  
  kSTO <- params["kSTO"]
  KS <- params["KS"]  
  KO2 <- params["KO2"]
  
  rate <- kSTO * (SO/(KO2 + SO)) * (SS/(KS + SS)) * XH
  
  dXH <- -rate
  dSS <- rate
  dSO <- -rate * (1 - KO2/(KO2 + SO))
  
  return(list(c(dXH, dSS, dSO)))
}

# Parameters
params <- c(kSTO = 10, KS = 10, KO2 = 0.5)

# Initial conditions 
state <- c(XH = 30, SS = 50, SO = 2) # Initial state vector

# Time steps
t <- seq(0, 5, by=0.05) 

# Numerical integration
library(deSolve) 
out <- ode(y = state, times = t, func = aero_storage, parms = params)

# Results
plot(out[,1], out[,2], type="l", 
     xlab="Time (days)", ylab="Concentration (g COD/m3)")
lines(out[,3], col="blue")  
legend("topright", legend=c("XH", "SS", "SO"), lty=1, col=1:3)

# Define the right-tail probability of interest $\alpha=0.001$
right_tail_p <- 0.05

# Define the vectors storing the indices corresponding to numerator (n1) and denominator (n2, row)
# degrees of freedom for $F(\alpha, n_1, n_2)$. Note that Inf corresponds to $\infty$.

n1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 24, 30, 40, 60, 70, 80, 90, 100, 108, 110, 120, Inf)
n2 <- c(1:30, 40, 60, 70, 80, 90, 100, 108, 110, 120, Inf)

# Define precision (4-decimal point accuracy)
options(digits=4)

# Generate an empty matrix of critical f-values
f_table <- matrix(ncol=length(n1), nrow=length(n2))

# Use the The F Distribution quantile function to fill in the matrix values in a nested 2-loop
# Recall that the density (df), distribution function (pf), quantile function (qf) and random generation (rf) for the F distribution

for (i in 1:length(n2)){
  for (j in 1:length(n1)){
    f_table[i,j] <- qf(right_tail_p, n1[j], n2[i], lower.tail = FALSE)
  }
} 

# Print results
f_table

# label rows and columns
rownames(f_table) <- n2; colnames(f_table) <- n1

# save results to a file
write.table(f_table, file="C:/Users/Kazem Kamrani/Desktop/R/f.txt")


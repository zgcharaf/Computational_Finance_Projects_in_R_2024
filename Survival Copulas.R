# Load necessary libraries
if (!requireNamespace("copula", quietly = TRUE)) {
  install.packages("copula")
}
library(copula)

correlation = 0.5 # Set the correlation coefficient
normal_copula <- normalCopula(correlation, dim = 2)

# Generate samples
set.seed(271)
U <- rCopula(1000, copula = normal_copula) # Sample from the normal copula
V <- 1 - U # Transform to sample from the survival normal copula

# Scatter plot for the normal copula
plot(U, xlab = expression(U[1]), ylab = expression(U[2]), main = "Normal Copula")

# Scatter plot for the survival normal copula
plot(V, xlab = expression(V[1]), ylab = expression(V[2]), main = "Survival Normal Copula")

# If you want to plot density or contour plots, consider using contour from base R or similar functions
library(MASS)  # For the kde2d function

# Generate density estimates for the normal copula
density_normal_copula <- kde2d(U[,1], U[,2], n=100)

# Plot the density contour for the normal copula
contour(density_normal_copula, main="Density Contour of Normal Copula", xlab="U1", ylab="U2", col.axis='blue', col.lab='blue')

# Generate density estimates for the survival normal copula
density_survival_copula <- kde2d(V[,1], V[,2], n=100)

# Plot the density contour for the survival normal copula
contour(density_survival_copula, main="Density Contour of Survival Normal Copula", xlab="V1", ylab="V2", col.axis='red', col.lab='red')


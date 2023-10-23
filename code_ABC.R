# ABC fitting for the SHEPPARD Model
# Author: M. Entezami

# Clear the workspace
rm(list = ls())

# Load required libraries
library(rstudioapi)
library(dplyr)

# Set the working directory to the location of the currently active script
setwd(dirname(getActiveDocumentContext()$path))

#### Data Loading and Preprocessing ####
Sim <- read.csv("model_int.csv")
# Adjust 'Step' and filter data
Sim$Step <- as.integer((Sim$Step + 10)/24)
Sim <- filter(Sim, Step == 40)
Sim <- filter(Sim, sheep_vaccine_cov_alt == 0.8 & dog_deworming_cov_alt == 0.65)
# Rename columns for clarity
Sim <- Sim %>% rename(
  step = Step,
  prevS = sheep_prev,
  prevD = dog_prev,
  betaSvec = sheep_consumption_rate,
  betaDvec = dog_consumption_rate,
  Shet = sheep_het,
  Dhet = dog_het
)

#### Fitting Procedure ####
# Define target values for dog and sheep prevalence
data <- data.frame(0.12,0.56)
colnames(data) <- c('prevD', 'prevS')

# Calculate the number of repeat points (M)
M <- nrow(Sim)
# Compute Empirical standard deviation
k <- apply(Sim[,c("prevD", "prevS")], 2, sd)
# Calculate the distance to the target
rho <- sqrt((Sim$prevD/k["prevD"] - data[1,1]/k["prevD"])^2 + (Sim$prevS/k["prevS"] - data[1,2]/k["prevS"])^2)
Sim$rho <- rho

# Save the scaled simulations to a CSV file
write.csv(Sim, 'Simscale1.csv')

# Filter out simulations based on rho value
N = 200
prop <- N/M
error <- quantile(Sim$rho, probs = prop)
SimKept <- subset(Sim, Sim$rho < error)
write.csv(SimKept$RunId, 'KeptIDs.csv')

# Compute weights for the kept simulations
weight <- (1/error) * (1 - (SimKept$rho/error)^2)

# Perform weighted linear regressions
lbetaD <- lm(betaDvec ~ prevD + prevS, SimKept, weights = weight)
lbetaS <- lm(betaSvec ~ prevD + prevS, SimKept, weights = weight)
lbetaH <- lm(Dhet ~ prevD + prevS, SimKept, weights = weight)
lbetaJ <- lm(Shet ~ prevD + prevS, SimKept, weights = weight)

# Predict values using the regression models
predbetaD <- predict(lbetaD, data)
predbetaS <- predict(lbetaS, data)
predbetaH <- predict(lbetaH, data)
predbetaJ <- predict(lbetaJ, data)

# Correct parameter values
cbetaD <- SimKept$betaDvec - lbetaD$fitted.values + predbetaD
cbetaS <- SimKept$betaSvec - lbetaS$fitted.values + predbetaS
cbetaH <- SimKept$Dhet - lbetaH$fitted.values + predbetaH
cbetaJ <- SimKept$Shet - lbetaJ$fitted.values + predbetaJ

#### Visualization ####
# For each parameter, plot the prior and posterior distributions, and display relevant statistics

#kept priors and parameters
plot(density(SimKept$betaDvec),col='red', main="Prior and Posterior Distributions of Dog Consumption", xlim = c(0, 1))
lines(density(Sim$betaDvec), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)

min(SimKept$betaDvec)
max(SimKept$betaDvec)
quantile(SimKept$betaDvec, c(0.05, 0.5, 0.95))

plot(density(SimKept$betaSvec),col='red', main="Prior and Posterior Distributions of Sheep Consumption", xlim = c(0, 0.00015))
lines(density(Sim$betaSvec), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)  

min(SimKept$betaSvec)
max(SimKept$betaSvec)
quantile(SimKept$betaSvec, c(0.05, 0.5, 0.95))

plot(density(SimKept$Dhet),col='red', main="Prior and Posterior Distributions of Dog Heterogeneity", xlim = c(0, 0.15))
lines(density(Sim$Dhet), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)

min(SimKept$Dhet)
max(SimKept$Dhet)
quantile(SimKept$Dhet, c(0.05, 0.5, 0.95))

plot(density(SimKept$Shet),col='red', main="Prior and Posterior Distributions of Sheep Heterogeneity", xlim = c(0, 1))
lines(density(Sim$Shet), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)

min(SimKept$Shet)
max(SimKept$Shet)
quantile(SimKept$Shet, c(0.05, 0.5, 0.95))

plot(SimKept$betaSvec, SimKept$betaDvec,col='red', main="Prior and Posterior distributions of sheep: Scale 10", xlim = c(0, 0.001))

#corrected priors and parameters
plot(density(cbetaD),col='red', main="Prior and Posterior Distributions of Dog Consumption", xlim = c(0, 1))
lines(density(Sim$betaDvec), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)

min(cbetaD)
max(cbetaD)
quantile(cbetaD, c(0.05, 0.5, 0.95))

plot(density(cbetaS),col='red', main="Prior and Posterior Distributions of Sheep Consumption", xlim = c(0, 0.00015))
lines(density(Sim$betaSvec), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)  

min(cbetaS)
max(cbetaS)
quantile(cbetaS, c(0.05, 0.5, 0.95))

plot(density(cbetaH),col='red', main="Prior and Posterior Distributions of Dog Heterogeneity", xlim = c(0, 0.15))
lines(density(Sim$Dhet), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)

min(cbetaH)
max(cbetaH)
quantile(cbetaH, c(0.05, 0.5, 0.95))

plot(density(cbetaJ),col='red', main="Prior and Posterior Distributions of Sheep Heterogeneity", xlim = c(0, 1))
lines(density(Sim$Shet), col='blue')
legend('topright', legend=c("prior", "posterior"),
       col=c("blue", "red"), lty=1:1, cex=0.6)
min(cbetaJ)
max(cbetaJ)
quantile(cbetaJ, c(0.05, 0.5, 0.95))
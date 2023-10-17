######################################
#### ABC fitting example for Elena ####
#######################################
# By J. Prada
rm(list = ls())
library(rstudioapi)
library(dplyr)
#### fitting ####
setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory
## In this example, I'm fitting 4 variables (fIgAp, fFEC, fvIgAp & fvFEC)
## which are model outputs.
## And I have 4 model parameters that I can change (lmr1, lmr2, lvr1, lvr2)
## You run multiple times your model sampling from parameter space uniformly
## i.e. set a value for lmr1 ... lvr2 and get model outputs
## We save the values of lmr1 ... lvr2 and the outputs on a file called "Sim"
## Each row is a different simulation (different parameters and outputs)
Sim <- read.csv("model_int.csv")
Sim$Step <- as.integer((Sim$Step + 10)/24)
Sim <- filter(Sim, Step == 40)
Sim <- filter(Sim, sheep_vaccine_cov_alt == 0.8 & dog_deworming_cov_alt == 0.65)
Sim <- Sim %>% rename(step = Step,
                      prevS = sheep_prev,
                      prevD = dog_prev,
                      betaSvec = sheep_consumption_rate,
                      betaDvec = dog_consumption_rate,
                      Shet = sheep_het,
                      Dhet = dog_het)
# You need to define your Summary Statistics - in this case 4 #
## i.e. what you are fitting to
data <- data.frame(0.12,0.56) #target values (for me my 2 prevalence)
colnames(data)<- c('prevD','prevS') #naming the columns

# M points generated # M number of repeats 
# Note that Sim is your model outputs dataframe
# M is thus the number of runs you did of your model
M <- nrow(Sim)
# Calculate the Empirical standard deviation #
k <- apply(Sim[,c("prevD", "prevS")],2,sd)
# Define rho(S(x),S(y)) #
# This is basically the distance to your "target"
rho<-sqrt((Sim$prevD/k["prevD"]-data[1,1]/k["prevD"])^2+(Sim$prevS/k["prevS"]-data[1,2]/k["prevS"])^2)
Sim$rho<-rho
write.csv(Sim,'Simscale1.csv')
# We keep the lowest rho, N=1000 #
## i.e. best fitting ones
N=200
prop<-N/M
error <- quantile(Sim$rho,probs=prop)#take 0.1 lowest rows
SimKept <- subset(Sim,Sim$rho<error)#keep ones below threshold
write.csv(SimKept$RunId, 'KeptIDs.csv')
# Weight the values saved #
weight<-(1/error)*(1-(SimKept$rho/error)^2)

# Apply the weighted linear regression for the 4 fitted parameters E(theta|S(x))
lbetaD <- lm(betaDvec~prevD+prevS,SimKept,weights=weight)
lbetaS <- lm(betaSvec~prevD+prevS,SimKept,weights=weight)
lbetaH <- lm(Dhet~prevD+prevS,SimKept,weights=weight)
lbetaJ <- lm(Shet~prevD+prevS,SimKept,weights=weight)

# Calculate E(theta|S(y))
predbetaD<-predict(lbetaD,data)
predbetaS<-predict(lbetaS,data)
predbetaH<-predict(lbetaH,data)
predbetaJ<-predict(lbetaJ,data)

# Correct the parameters
cbetaD<-SimKept$betaDvec-lbetaD$fitted.values+predbetaD
cbetaS<-SimKept$betaSvec-lbetaS$fitted.values+predbetaS
cbetaH<-SimKept$Dhet-lbetaH$fitted.values+predbetaH
cbetaJ<-SimKept$Shet-lbetaJ$fitted.values+predbetaJ#

#### analysis ####

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
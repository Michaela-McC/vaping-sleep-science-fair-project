# Add the memory and reaction

setwd("~/Servicios/QL/Mentees/MichaelaMcCormack")
library(dplyr)
library(ggplot2)
library(R2jags)
library(mcmcplots)
source("R/logit.R")
source("R/expit.R")
set.seed(123)

data(Sim)
Sim$Attitude <- factor(Sim$Attitude,c("Nerd","Wild"))
nObs <- nrow(Sim)

# Check the 
table(Sim[,c("Attitude","Vape")])
boxplot(Mem ~ Vape, data=Sim)
boxplot(Mem ~ Attitude, data=Sim)
boxplot(Rec ~ Vape, data=Sim)
boxplot(Rec ~ Attitude, data=Sim)

# What about randomized response for [Vape|Attitude]
# See Link and Barker p. 165--182
BUGSMdl <- function() {
  # Noninformed constants
  NITau <- 0.001
  NIVar <- 0.01
  
  # Likelihoods
  for (iObs in 1:nObs) {
    Vape[iObs] ~ dbern(pVape[Attitude[iObs]])
    VapeResp[iObs] ~ dbern(Vape[iObs] + pResp * (1 - Vape[iObs]))
    lgtMem[iObs] ~ dnorm(lMemReg[Vape[iObs] + 1], lMemTau)
    lgtReact[iObs] ~ dnorm(lReactReg[Vape[iObs] + 1], lReactTau)
  }
  
  # Priors
  for (iAttitude in 1:2) {
    pVape[iAttitude] ~ dbeta(1, 1)
    lMemReg[iAttitude] ~ dnorm(0, NITau)
    lReactReg[iAttitude] ~ dnorm(0, NITau)
  }
  
  # lMemTau[iAttitude] ~ dgamma(NIVar, NIVar)
  # lReactTau[iAttitude] ~ dgamma(NIVar, NIVar)
  lMemTau ~ dgamma(NIVar, NIVar)
  lReactTau ~ dgamma(NIVar, NIVar)
  
  # Convert the memory and reaction scores back to percentages in R
}

# Proportion of time they do not answer the sensitive question
# and respond yes
pRndRsp <- 0.25

iSim <- Sim %>%
  mutate(
    CoinFlip = as.logical(rbinom(nObs, 1, pRndRsp)),
    VapeResp = CoinFlip | Vape,
    lgtMem = logit(Mem),
    lgtReact = logit(Rec)
  )

Data <- list(
  nObs = nObs,
  pResp = pRndRsp,
  Attitude = as.integer(iSim$Attitude),
  Vape = ifelse(iSim$VapeResp, NA, 0),
  VapeResp = iSim$VapeResp,
  lgtMem = iSim$lgtMem,
  lgtReact = iSim$lgtReact
)

Parm <- c("pVape","lMemReg","lMemTau","lReactReg","lReactTau")
cParm <- paste("pV", c("Nerd", "Wild"), sep = ".")

# inits <- function() {
#  list(N = c(apply(C, 1, sum), rep(10, Sites - n)))
# }

MCMC <- jags(
  model = BUGSMdl,
  # init = inits,
  data = Data,
  param = Parm,
  n.chain = 4,
  n.iter = 20000,
  n.burnin = 5000,
  n.thin = 1
)

# traceplot(MCMC)
traplot(MCMC)
denplot(MCMC)  #, parms=c("")
print(MCMC)


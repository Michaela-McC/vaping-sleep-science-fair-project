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
cAttitude <- c("Nerd", "Wild")
Sim$Attitude <- factor(Sim$Attitude, cAttitude)
nObs <- nrow(Sim)

# Check the
table(Sim[, c("Attitude", "Vape")])
boxplot(Mem ~ Vape, data = Sim)
boxplot(Mem ~ Attitude, data = Sim)
boxplot(Rec ~ Vape, data = Sim)
boxplot(Rec ~ Attitude, data = Sim)

# What about randomized response for [Vape|Attitude]
# See Link and Barker p. 165--182
BUGSMdl <- function() {
  # Noninformed constants
  NIBeta <- 1000
  
  # Likelihoods
  for (iObs in 1:nObs) {
    Vape[iObs] ~ dbern(pVape[Attitude[iObs]])
    VapeResp[iObs] ~ dbern(Vape[iObs] + pResp * (1 - Vape[iObs]))
    Mem[iObs] ~ dbeta(MemNu[Vape[iObs] + 1], MemNup[Vape[iObs] + 1])
    React[iObs] ~ dbeta(ReactNu[Vape[iObs] + 1], ReactNup[Vape[iObs] + 1])
  }
  
  # Priors
  for (iAttitude in 1:2) {
    pVape[iAttitude] ~ dbeta(1, 1)
    MemNu[iAttitude] ~ dunif(0, NIBeta)
    MemNup[iAttitude] ~ dunif(0, NIBeta)
    ReactNu[iAttitude] ~ dunif(0, NIBeta)
    ReactNup[iAttitude] ~ dunif(0, NIBeta)
  }
  
  # Convert the memory and reaction scores back to percentages 
  # https://en.wikipedia.org/wiki/Beta_distribution
  for (iParm in 1:2) {
    PctMem[iParm] <- 100 * MemNu[iParm] / (MemNu[iParm] + MemNup[iParm])
    PctMemSD[iParm] <- 100 * sqrt(MemNu[iParm] * MemNup[iParm] /
                                    (MemNu[iParm] + MemNup[iParm]) ^ 2 /
                                    (MemNu[iParm] + MemNup[iParm] + 1))
    PctReact[iParm] <-
      100 * ReactNu[iParm] / (ReactNu[iParm] + ReactNup[iParm])
    PctReactSD[iParm] <- 100 * sqrt(ReactNu[iParm] * ReactNup[iParm] /
                                      (ReactNu[iParm] + ReactNup[iParm]) ^ 2 /
                                      (ReactNu[iParm] + ReactNup[iParm] + 1))
  }
}

# Proportion of time they do not answer the sensitive question
# and respond yes
pRndRsp <- 0.25

iSim <- Sim %>%
  mutate(CoinFlip = as.logical(rbinom(nObs, 1, pRndRsp)),
         VapeResp = CoinFlip | Vape)

Data <- list(
  nObs = nObs,
  pResp = pRndRsp,
  Attitude = as.integer(iSim$Attitude),
  Vape = ifelse(iSim$VapeResp, NA, 0),
  VapeResp = iSim$VapeResp,
  Mem = iSim$Mem,
  React = iSim$Rec
)

Parm <- c("pVape", "PctMem", "PctMemSD", "PctReact", "PctReactSD")
cParm <- paste("pV", cAttitude, sep = ".")

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



setwd("~/Servicios/QL/Mentees/MichaelaMcCormack")
library(dplyr)
library(ggplot2)
library(R2jags)
data(Sim)
nObs <- nrow(Sim)
# What about randomized response for [Vape|Attitude]
# See Link and Barker p. 165--182
BUGSMdl <- function() {
  for (iObs in 1:nObs) {
    Vape[iObs] ~ dbern(pVape[Attitude[iObs]])
    VapeResp[iObs] ~ dbern(Vape[iObs] + pResp * (1 - Vape[iObs]))
  }
  
  for (iAttitude in 1:2) {
    pVape[iAttitude] ~ dbeta(1, 1)
  }
}

# Proportion of time they do not answer the sensitive question
# and respond yes
pRndRsp <- 0.25

iSim <- Sim %>%
  select(Attitude, Vape, CoinFlip, VapeResp) %>%
  mutate(CoinFlip = as.logical(rbinom(nObs, 1, pRndRsp)),
         VapeResp = CoinFlip | Vape)

Data <- list(
  nObs = nObs,
  pResp = pRndRsp,
  Attitude = as.integer(iSim$Attitude),
  Vape = ifelse(iSim$VapeResp, NA, 0),
  VapeResp = iSim$VapeResp
)

Parm <- c("pVape")
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
  n.iter = 11000,
  n.burnin = 1000,
  n.thin = 1
)

traceplot(MCMC)
plot(MCMC)
print(MCMC)

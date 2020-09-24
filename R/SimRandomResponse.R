# Get a start with R
# http://mosaic-web.org/go/Master-Starting.pdf

setwd("~/Servicios/QL/Mentees/MichaelaMcCormack")
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(R2jags)

cGender <- c("M", "F")
cAttitude <- c("Nerd", "Wild")

Data <-
  readxl::read_xlsx("ExtData/Cognitive Test Data w_ Dates.xlsx",
                    skip = 3)
Data <- readr::read_csv("ExtData/Cognitive Test Data w_ Dates.csv",
                        skip = 0)

# Make a fake data set to show how to analyze the randomized response
# Will need the expectation maximization
nObs <- 66  # Think this is the responses
pRndRsp <- 0.2

#
Sim <- tibble::tibble(
  # Half male-female exactly
  Gender = sample(rep(cGender, length = nObs), nObs),
  Attitude = "Nerd",
  Vape = FALSE,
  CoinFlip = FALSE,
  VapeResp = NA   # Response to the randomized response question
)

# Wildness depends on Gender
pFWild <- 0.20
pMWild <- 0.85

Sim[Sim$Gender == "M", "Attitude"][sample(1:(nObs / 2), round(nObs / 2 * pMWild)), 1] <-
  "Wild"
Sim[Sim$Gender == "F", "Attitude"][sample(1:(nObs / 2), round(nObs / 2 * pFWild)), 1] <-
  "Wild"

# Vaping depends on wildness
nWild <- sum(Sim$Attitude == "Wild")
pWildVape <- 0.90
pNerdVape <- 0.05

Sim[Sim$Attitude == "Wild", "Vape"][sample(1:nWild, round(nWild * pWildVape)), 1] <-
  TRUE
Sim[Sim$Attitude == "Nerd", "Vape"][sample(1:(nObs - nWild), round(nObs /
                                                                     2 * pNerdVape)), 1] <-
  TRUE

# Make factors of the variables
Sim <- Sim %>%
  mutate(Gender = factor(Gender, cGender),
         Attitude = factor(Attitude, cAttitude))

# Vaping is only affected by gender
table(Sim[, c("Attitude", "Vape")])

Mdl.AG <- glm(Attitude ~ -1 + Gender,
              data = Sim,
              family = binomial(link = "logit"))
summary(Mdl.AG)

# Gender has an indirect relation to vaping
Mdl.VG <- glm(Vape ~ -1 + Gender,
              data = Sim,
              family = binomial(link = "logit"))
summary(Mdl.VG)

Mdl.VA <- glm(Vape ~ -1 + Attitude,
              data = Sim,
              family = binomial(link = "logit"))
summary(Mdl.VA)

# Given attitude, gender is irrelevant
Mdl.VAG <- glm(Vape ~ -1 + Attitude * Gender,
               data = Sim,
               family = binomial(link = "logit"))
summary(Mdl.VAG)

# VA model has the best fit with the least parameters
AIC(Mdl.VG, Mdl.VA, Mdl.VAG)

# Randomized Response

Sim$VapeResp <- Sim$Vape

# No matter what the response would have been answer TRUE
Idx <- sample(1:nObs, round(nObs * pRndRsp))
Sim[Idx, "CoinFlip"] <- TRUE
Sim[Idx, "VapeResp"] <- TRUE

# Memory and reaction are affected by vaping
NuMem <- 22
NupMem <- 2
curve(dbeta(x, NuMem, NupMem), 0, 1)


NuMemVape <- 2
NupMemVape <- 6
curve(dbeta(x, NuMemVape, NupMemVape), 0, 1)

NuRec <- 3
NupRec <- 15
curve(dbeta(x, NuRec, NupRec), 0, 1)

NuRecVape <- 22
NupRecVape <- 14
curve(dbeta(x, NuRecVape, NupRecVape), 0, 1)

Sim <- Sim %>%
  mutate(
    Mem = ifelse(
      Vape,
      rbeta(nObs, NuMemVape, NupMemVape),
      rbeta(nObs, NuMem, NupMem)
    ),
    Rec = ifelse(
      Vape,
      rbeta(nObs, NuRecVape, NupRecVape),
      rbeta(nObs, NuRec, NupRec)
    )
  )

# 
 table(Sim[,c("Gender","Vape","Attitude")])
Sim %>% 
  group_by(Gender,Attitude) %>%
  summarize(Vape = 100 * sum(Vape))
  
  # ggplot(aes(x=Gender,y=lifeExp, fill=continent)) +
  # geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) 

# Gender and attitude  (Figure out how to explain with mosaic plots)
ggplot(data = Sim) +
  geom_mosaic(aes(x = product(Gender), fill=Vape), 
              na.rm=TRUE, divider=mosaic("v")) +  
  labs(x = "Gender ", title="Gender")

ggplot(data = Sim) +
  geom_mosaic(aes(x = product(Vape, Attitude), fill=Vape, conds=product(Gender)), 
              na.rm=TRUE, divider=mosaic("v")) +  
  labs(x = "Vape ", y = "Attitude", title='f(Vape?, Attitude| Gender)')

ggplot(data = Sim) +
  geom_mosaic(aes(x = product(Vape, Attitude), fill=Vape, conds=product(Gender)), 
              na.rm=TRUE, divider=mosaic("v")) +  
  labs(x = "Vape ", title='f(Vape?, Attitude| Gender)')



 Mdl.MVA <- glm(Mem ~ Vape * Attitude, 
#            family=binomial(link="logit"),
            family=gaussian(link="logit"),
            data = Sim)
 summary(Mdl.MVA)
 
 
 Mdl.RVA <- glm(Rec ~ Vape * Attitude, 
                #            family=binomial(link="logit"),
                family=gaussian(link="logit"),
                data = Sim)
 summary(Mdl.RVA)
 
 # What about randomized response for [Vape|Attitude]
 # See Link and Barker p. 165--182
 BUGSMdl <- function(){
   for(iObs in 1:nObs){
     Vape[iObs] ~ dbern(pVape)
     VapeResp[iObs] ~ dbern(Vape[iObs] + pResp * (1 - Vape[iObs]))
   }
   
   pVape ~ dbeta(1,1)
 }
 
 pResp <- seq(0.0,0.95,by=0.05)
 Results <- sapply(pResp,function(pRndRsp){
   # pRndRsp <- 0.2
 pRndRspSD <- sqrt(pRndRsp*(1-pRndRsp)/nObs)
 
 iSim <- Sim %>%
   select(Attitude, Vape, CoinFlip, VapeResp) %>%
   mutate(
     CoinFlip = as.logical(rbinom(nObs,1,pRndRsp)),
     VapeResp = CoinFlip | Vape
   )
 
 Data <- list(
   nObs = nObs,
   pResp = pRndRsp,
   Vape = ifelse(iSim$VapeResp, NA, 0),
   VapeResp = iSim$VapeResp
 )
 
 Parm <- c("pVape")
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
 return(c(Mean = MCMC$BUGSoutput$mean$pVape,
        SD = MCMC$BUGSoutput$sd$pVape))
 })
 
 Results <- tibble::tibble(
   pResp = pResp,
   pVape = Results[1,],
   pVapeSD = Results[2,]
 )
 
 Results %>%
   mutate(
     PctLoss = 100 * (pVapeSD - min(pVapeSD))/min(pVapeSD),
     PctYes = 100 * pResp
   ) %>%
   ggplot(aes(x=PctYes,y=PctLoss)) +
   geom_line() +
   ggtitle("Percent Increase of Randomize Response SD") +
   xlab("Proportion to say yes") + 
   ylab("Rel SD Increace%") 
 
 
 
 traceplot(MCMC)
 plot(MCMC)
 print(MCMC)
 
 save(MCMC,Data,Sim,file="Data/pRespVSSD.RData")
 save(Sim,file="Data/Sim.RData")
 
 
 
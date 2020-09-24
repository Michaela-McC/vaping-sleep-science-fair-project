 logit<-function(p)
	ifelse(p<=0|p>=1,NA,log(p/(1-p)))

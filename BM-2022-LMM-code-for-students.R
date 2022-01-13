# Mixed-effect linear model: MLE analysis using R

rm(list=ls())

n=100	# number of observed individuals
M=5		# number of measurements 
N=n*M	# balanced design (M is constant over the individuals)

occasions=c(0,1,3,6,12) # months at which each individual is observed
set.seed(12345)

load("writtentest2022.RData")
attach(data)
str(data)

library(lme4)

#lme.REML=lmer(y~(1|ID)+times.long+(0+times.long|ID)+gender.long)
lme.REML=lmer(y~(1|ID)+times.long+(0+times.long|ID)+gender.long+weight.long)
fixef(lme.REML)
ranef(lme.REML)
coef(lme.REML)

str(coef(lme.REML))
cbind(ranef(lme.REML)$ID,rep(0,n))+t(fixef(lme.REML))[rep(1,n),]

print(lme.REML)


cat("model {

# Priors

for(k in 1:K){
	beta[k]~dnorm(0,0.001)
}	

tau.e~dgamma(0.001,0.001)
sigma.e <- 1/sqrt(tau.e)

tau.b0~dgamma(0.001,0.001)
sigma.b0 <- 1/sqrt(tau.b0)

tau.b1~dgamma(0.001,0.001)
sigma.b1 <-1 /sqrt(tau.b1)

# Statistical (conditional) model

for(i in 1:n){
	b0[i]~dnorm(0,tau.b0)
	b1[i]~dnorm(0,tau.b1)
for(t in 1:M){
	mu[i,t] <- beta[1] + b0[i] + (beta[2]+b1[i])*times[i,t] + beta[3]*gender[i,t] + beta[4]*weight[i,t]
	y[i,t]~dnorm(mu[i,t],tau.e)
    }
    }

}",file="modelA.txt",fill=TRUE)

# Data preparation

K=ncol(data)-1

data.input=list(n=n,M=M,K=K,
                y=matrix(y,nrow=n,ncol=M,byrow=TRUE), # note that data are provided in a different format
                times=matrix(times.long,nrow=n,ncol=M,byrow=TRUE),
                gender=matrix(data[,"gender.long"],nrow=n,ncol=M,byrow=TRUE),
                weight=matrix(weight.long,nrow=n,ncol=M,byrow=TRUE)) 

# Initialization values

inits=list(
  list(b0=ranef(lme.REML)$ID[,1],b1=ranef(lme.REML)$ID[,2],
       beta=as.numeric(fixef(lme.REML)),
       tau.e=1/sigma(lme.REML)^2,
       tau.b0=1/as.data.frame(VarCorr(lme.REML))[1,"sdcor"]^2,
       tau.b1=1/as.data.frame(VarCorr(lme.REML))[2,"sdcor"]^2)
)

# Parameters to monitor

params=c("beta","sigma.e","sigma.b0","sigma.b1")

# Let us call JAGS

library(R2jags)

modelA.jags=jags(data=data.input,inits=inits,
                     parameters.to.save=params,
                     model.file="modelA.txt",
                     DIC=TRUE,n.chains=1,n.iter=21000,n.burnin=1000,n.thin=1)

print(modelA.jags,digits=3)


cat("model {

    # Priors
    
    for(k in 1:K){
    beta[k]~dnorm(0,0.001)
    }	
    
    tau.e~dgamma(0.001,0.001)
    sigma.e <- 1/sqrt(tau.e)
    
    tau.b0~dgamma(0.001,0.001)
    sigma.b0 <- 1/sqrt(tau.b0)
    
    # Statistical (conditional) model
    
    for(i in 1:n){
    b0[i]~dnorm(0,tau.b0)
    for(t in 1:M){
    mu[i,t] <- beta[1] + b0[i] + beta[2]*times[i,t] + beta[3]*gender[i,t] + beta[4]*weight[i,t]
    y[i,t]~dnorm(mu[i,t],tau.e)
    }
    }
    
    }",file="modelB.txt",fill=TRUE)

lme.REML2=lmer(y~(1|ID)+times.long+gender.long+weight.long)

# Initialization values

inits2=list(
  list(b0=ranef(lme.REML2)$ID[,1],
       beta=as.numeric(fixef(lme.REML2)),
       tau.e=1/sigma(lme.REML2)^2,
       tau.b0=1/as.data.frame(VarCorr(lme.REML2))[1,"sdcor"]^2)
)

# Parameters to monitor

params2=c("beta","sigma.e","sigma.b0")

modelB.jags=jags(data=data.input,inits=inits2,
                 parameters.to.save=params2,
                 model.file="modelB.txt",
                 DIC=TRUE,n.chains=1,n.iter=21000,n.burnin=1000,n.thin=1)




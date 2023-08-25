library(popbio)
library(plotrix)


#### Simple SurvivalExample__________________________________________________________________####

# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# survival estimate from MARK output
S <-0.94

# SE from MARK output
SE <-0.03

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(S,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))



#_________________________________________________________________________



#### Cumulative Survival Example__________####

# Daily survival
DSR<-c(0.985,0.99,0.924,0.976,0.9,0.92)
# SE of DSR
DSE<-c(0.008,0.01,0.005,0.009,0.007,0.009)

ndays<-length(DSR)
# Let's say we want cumulative survival to 30 days and associated 85% CIs

daydist<-matrix(NA,nrow=ndays,ncol=nsim) # Create empty matrix to store results

# Loop across days and nsims
for (n in 1:ndays){
  for (i in 1:nsim){daydist[n,i]<-betaval(DSR[n],DSE[n])}
}

# Now take the product of the DSRs to get cumulative survival for each simulation
cumdist<-apply(daydist,2,prod)
# Take a look at the resulting distribution
hist(cumdist,breaks=50)

# Now summarize
# cumulative survival
mean(cumdist)
# SE of cumulative survival
library(plotrix)
std.error(cumdist)
# 85% CI of cumulative survival
quantile(cumdist,probs=c(0.075,0.925))







#________________________________________________________________________

###LOSH Model Surival and Detection Constant Phi(.)p(.)__________________________#####

####LOSH WIthin Season Survival Weekly estimates from model_______________________________________________####

# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# survival estimate from MARK output
S <-0.9504

# SE from MARK output
SE <-0.0106

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(S,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))



####LOSH WIthin Season Detection_______________________________________________####

# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# detection estimate from MARK output
D <-0.7395

# SE from MARK output
SE <-0.0210

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(D,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))


####LOSH WIthin Season Survival Extrapolated across 15 resight weeks_______________________________________________####

# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# survival estimate 
S <-0.466219

# SE
SE <-0.077727

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(S,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))


















#####______________________________________________________________________________#####


####LOSH Model Phi(.)p(t) - Survival Constant and Detection Time Dependent________####

####LOSH WIthin Season Simple Survival_______________________________________________####

#_______________________________________________________________________________
# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# survival estimate from MARK output
S <-0.9574

# SE from MARK output
SE <-0.0101

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(S,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))


#LOSH WIthin Season Detection_______________________________________________###

#________________________________________________________________________________________


####LOSH WIthin Season Cumulative Detection_______________________________________________####

# Daily Detection
DDR<-c(0.6812,0.7826,0.8120,0.8333,0.8947,0.7619,0.5965,0.6426,0.8914,0.8090,0.7971,0.6578,0.7252,0.5745)
# SE of DSR
DSE<-c(0.1485,0.1108,0.0978,0.0879,0.0704,0.0831,0.0749,0.0681,0.0454,0.0514,0.0531,0.0617,0.0675,0.0710)

ndays<-length(DDR)
# Let's say we want cumulative survival to 30 days and associated 85% CIs

daydist<-matrix(NA,nrow=ndays,ncol=nsim) # Create empty matrix to store results

# Loop across days and nsims
for (n in 1:ndays){
  for (i in 1:nsim){daydist[n,i]<-betaval(DDR[n],DSE[n])}
}

# Now take the product of the DSRs to get cumulative survival for each simulation
cumdist<-apply(daydist,2,prod)
# Take a look at the resulting distribution
hist(cumdist,breaks=50)

# Now summarize
# cumulative survival
mean(cumdist)
# SE of cumulative survival
library(plotrix)
std.error(cumdist)
# 85% CI of cumulative survival
quantile(cumdist,probs=c(0.075,0.925))








####LOSH Model Phi(.)p(.) - Survival Detection Constant________####

####LOSH Annual Simple Survival_______________________________________________####

#_______________________________________________________________________________
# Monte Carlo simulations for 85% CIs from mean and SE, C. 

# survival estimate from MARK output
S <-0.4333

# SE from MARK output
SE <-0.0522

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(S,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))


#LOSH Annual Season Detection_______________________________________________###

#________________________________________________________________________________________


####LOSH Annual Simple Detection_______________________________________________####

# survival estimate from MARK output
D <-1.000

# SE from MARK output
SE <-0.000

# number of simulations you want - make this large, mean will be very similar to truth when sufficiently large
nsim<-10000
# Create beta distribution with mean S and SE=SE
library(popbio) # will need function betaval in this package

dist<-double() # create empty object to store results

# Create distribution of values
for (i in 1:nsim){dist[i]<-betaval(D,SE)}
# take a look at the distribution
hist(dist,breaks=20)

# now get 85% CIs from it
quantile(dist,probs=c(0.075,0.925))



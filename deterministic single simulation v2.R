#--------------------------------------------------------
# Deterministic single simulation
#--------------------------------------------------------
source('deterministic functions general.R')
source('deterministic function single simulation v2.R')
raw <- read.csv('data.csv') # 1991 to 2020 inc
#--------------------------------------------------------
# Interpolate missing data points (Not be needed for summary stats).
#--------------------------------------------------------
data <- raw
for(c in 2:ncol(raw))data[,c] <- approx(raw[,c],xout=1:nrow(raw), rule=2:2)$y
#--------------------------------------------------------
# Under the deterministic model (no stochasticity) we can either:
# 1) Set each unknown to have the same value for all years (just 1 parameter for each unknown)
# 2) Set each unknown to have a different value for all years (as many parameters for each unknown as there are years). This level of model complexity is hard to justify, but worth demonstrating
#--------------------------------------------------------
# Run a single simulation of the entire timeseries
#--------------------------------------------------------
# data must be a data.frame where each row is a year
# parameters must be a single row data frame

N.sims <- 10000

parameters <- data.frame(
	willingness = runif(N.sims, 0, 1),
	schemes.available = runif(N.sims, 20000, 130000), # consider a log uniform distribution
	mean.area.per.agreement = runif(N.sims, 0.01,0.04),
	maximum.land.area = runif(N.sims, 1, 100000), # consider a log uniform distribution
	fc.proportion = runif(N.sims, 0, 1),
	fc.new.proportion = runif(N.sims, 0, 1),
	new.plants.per.area = runif(N.sims, 1, 2.5),
	forest.area.initialisation = runif(N.sims, 1200, 1400), # consider a log uniform distribution
	farm.woodland.area.initialisation = runif(N.sims, 100, 1000), # consider a log uniform distribution
	area.per.green.tonne = runif(N.sims, 0, 0.1),
	woodland.proportion.felled = runif(N.sims, 0, 1),
	intended.area.of.extra.farm.woodland.planting.and.restocking <- runif(N.sims, 0, 100) # consider a log uniform distribution
	)
#--------------------------------------------------------
# MAIN LOOP
# data must be a data.frame where each row is a year
# parameters must be a single row data frame
# very slow currently. c. 6 mins per 10,000 sims.
# Suggest running 1 million ovenight (13 hrs) and saving all objects.
# # Then top skim the best 100, 200, 500, and 1000
#--------------------------------------------------------
distances <- numeric(N.sims)
for(n in 1:N.sims){ 
  sim <- single.simulation.v2(data=data, parameters=parameters[n,])
  distances[n] <- distance.v2(sim, raw)
  print(n)
}
#--------------------------------------------------------
# PROCESSING
#--------------------------------------------------------
# prior distributions
par(mfrow=c(3,4), mar=c(4,4,1,1))
for(i in 1:12)hist(parameters[,i],main=names(parameters)[i])

# best top skim of simulations
best.i <- order(distances)[1:100]
best <- parameters[best.i,]
par(mfrow=c(3,4), mar=c(4,4,1,1))
for(i in 1:12){
  h <- hist(parameters[,i],plot=F, breaks = 10)
  hist(best[,i],main=names(best)[i],breaks=h$breaks, col="blue")
}

#--------------------------------------------------------
sim <- single.simulation.v2(data=data, parameters=parameters)
#--------------------------------------------------------






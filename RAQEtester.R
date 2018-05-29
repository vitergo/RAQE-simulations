#Save all files in a single working directory.
#Then, set the working directory as the directory where you saved the files.
setwd("/home/victor/Documents/R Works/RAQE") #directory where functions are located

#Run all functions from the files within the wd.
source("GetDist.R")
source("GetQuantile.R")
source("ksamplesimulator.R")
source("functionstofit.R")
source("RAQE.R")
source("RAQEperformance.R")

#-------------------------------------------------------------------
#Simulations starts here

#Define scenario
p = 0.975
upper.tail = TRUE
n = c(300, 300)
mu = c(0, 0)
sdev = c(1, 1)
distr = c("Normal", "LogNormal", "Gamma", "Weibull")[2]
par.location = 0
par.scale = 1
par.shape = 1
replicates = 1000
tail.percentage = 40
use.weights = T

#Testing one sample at the time
data = simSamples(n = n, mu = mu, sigma = sdev, dist = distr, par.location = par.location, par.scale = par.scale, par.shape = par.shape)
hist(data[,1])
RAQE(data = data, p = p, upper.tail = upper.tail, tail.percentage = tail.percentage, plot.empirical = TRUE, use.weights = use.weights)


# Test the preformance of one scenario at the time
scenario = RAQEperformance(p, upper.tail, tail.percentage, n, mu, sdev, distr, par.location = par.location, par.scale = par.scale, par.shape = par.shape, replicates, use.weights)
scenario

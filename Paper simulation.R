#Save all files in a single working directory.

#Preliminaries: Set working directory and file names to save results
setwd("/home/victor/Documents/R Works/RAQE") #path where functions are located. Don't use "\"
FileNameR = "RAQE_simResults_20180529.R" #Name to save as R object
FileNameCSV = "RAQE_simResults_20180529.CSV" #Name to save as CSV file


#Run all functions from the files within the wd.
source("GetDist.R")
source("GetQuantile.R")
source("ksamplesimulator.R")
source("functionstofit.R")
source("RAQE.R")
source("RAQEperformance.R")

#Scenarios sets
replicates = 1000
s.p = c(0.9, 0.95, 0.975, 0.99, 0.9986501, 0.999)
upper.tail = TRUE
s.n = list(n1 = c(50),
           n2 = c(100),
           n3 = c(300),
           n4 = c(50,50),
           n5 = c(100,100),
           n6 = c(300,300))
mu = list(mu1 = c(0),
          mu2 = c(0),
          mu3 = c(0),
          mu4 = c(0,0),
          mu5 = c(0,0),
          mu6 = c(0,0))
sdev = list(sdev1 = c(1),
            sdev2 = c(1),
            sdev3 = c(1),
            sdev4 = c(1,1),
            sdev5 = c(1,1),
            sdev6 = c(1,1))
s.distr = c("Normal", "LogNormal", "Gamma", "Weibull")[3]
par.location = 0
s.par.scale = c(0.5, 1, 1.5)
s.par.shape = c(0.5, 1, 3, 10)
s.tail.percentage = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
s.use.weights = c(TRUE, FALSE)

#Initialize data frame
n.normal.needed = 1 * length(s.par.scale[1]) * length(s.par.shape[1]) * length(s.p) * length(upper.tail) * length(s.n) * length(par.location) * length(replicates) * length(s.tail.percentage) * length(s.use.weights)
n.lognormal.needed = 1 * length(s.par.scale) * length(s.par.shape[1]) * length(s.p) * length(upper.tail) * length(s.n) * length(par.location) * length(replicates) * length(s.tail.percentage) * length(s.use.weights)
n.gamma.needed =   1 * length(s.par.scale[1]) * length(s.par.shape) * length(s.p) * length(upper.tail) * length(s.n) * length(par.location) * length(replicates) * length(s.tail.percentage) * length(s.use.weights)
n.weibull.needed = 1 * length(s.par.scale[1]) * length(s.par.shape) * length(s.p) * length(upper.tail) * length(s.n) * length(par.location) * length(replicates) * length(s.tail.percentage) * length(s.use.weights)

n.scenarios = n.normal.needed + n.lognormal.needed + n.gamma.needed + n.weibull.needed

NAs = rep(NA, n.scenarios)
Results = data.frame(replicates = NAs,
                     distribution = NAs,
                     EX = NAs,
                     VarX = NAs,
                     location = NAs,
                     scale = NAs,
                     shape = NAs,
                     kgroups = NAs,
                     p = NAs,
                     n = NAs,
                     tail.percentage = NAs,
                     weights = NAs,
                     true.q = NAs,
                     meanQ = NAs,
                     bias = NAs,
                     sdQ = NAs,
                     MAD = NAs,
                     nExponential = NAs,
                     NGumbelmax = NAs,
                     NGumbelmin = NAs,
                     nLogit = NAs,
                     nWeibull = NAs)

#Run sets
i = 0
s.par.shape.temp = s.par.shape
s.par.scale.temp = s.par.scale
K = length(s.n) #number of levels in groups
for(distr in s.distr){
  
  #Restrictions due to a distribution
  
  #Shape parameter restrictions
  if(distr == "Normal" || distr == "LogNormal"){
    s.par.shape = 1
  }else{
    s.par.shape = s.par.shape.temp
  }
  
  #Scale parameter restrictions
  if(distr != "LogNormal"){
    s.par.scale = 1
  }else{
    s.par.scale = s.par.scale.temp
  }
  
  for(par.scale in s.par.scale){
    for(par.shape in s.par.shape){
      for(p in s.p){
        for(k in 1:K){
          for(tail.percentage in s.tail.percentage){
            for(use.weights in s.use.weights){
              i = i + 1
              scenario = RAQEperformance(p,
                                          upper.tail,
                                          tail.percentage,
                                          n = as.numeric(s.n[[k]]),
                                          mu = as.numeric(mu[[k]]),
                                          sdev = as.numeric(sdev[[k]]),
                                          distr,
                                          par.location = par.location,
                                          par.scale = par.scale,
                                          par.shape = par.shape,
                                          replicates,
                                          use.weights)
              
              #Scenario information
              Results$replicates[i] = replicates
              Results$distribution[i] = distr
              Results$EX[i] = mean(mu[[k]])
              Results$VarX[i] = mean(sdev[[k]])
              Results$location[i] = par.location
              Results$scale[i] = par.scale
              Results$shape[i] = par.shape
              Results$kgroups[i] = length(mu[[k]])
              Results$p[i] = p
              Results$n[i] = mean(s.n[[k]])
              Results$tail.percentage[i] = tail.percentage
              Results$weights[i] = use.weights
              
              #Scenario performance
              Results$true.q[i] = scenario$q.true
              Results$meanQ[i] = scenario$q.mean
              Results$bias[i] = scenario$q.bias
              Results$sdQ[i] = scenario$q.sd
              Results$MAD[i] = scenario$q.MAD
              
              tt = as.numeric(scenario$best.fit[names(scenario$best.fit) == "Exponential"])
              if(length(tt) == 0) tt = 0
              Results$nExponential[i] = tt
              
              tt = as.numeric(scenario$best.fit[names(scenario$best.fit) == "Gumbel.max"])
              if(length(tt) == 0) tt = 0
              Results$NGumbelmax[i] = tt
              
              tt = as.numeric(scenario$best.fit[names(scenario$best.fit) == "Gumbel.min"])
              if(length(tt) == 0) tt = 0
              Results$NGumbelmin[i] = tt
              
              tt = as.numeric(scenario$best.fit[names(scenario$best.fit) == "Logit"])
              if(length(tt) == 0) tt = 0
              Results$nLogit[i] = tt
              
              tt = as.numeric(scenario$best.fit[names(scenario$best.fit) == "Weibull"])
              if(length(tt) == 0) tt = 0
              Results$nWeibull[i] = tt
              
              if(i %% 50 == 0){
                save(Results, file = "RAQE_Results_temp.R")
              }
              print(paste("Progress = ", round(i / n.scenarios * 100, digits = 2),"%"))
            }
          }
        }
      }
    } 
  }
}
save(Results, file = FileNameR)
write.csv(Results, file = FileNameCSV, row.names = FALSE)

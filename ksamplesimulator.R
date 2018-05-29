#####################################################################################
#
#     k SAMPLES SIMULATOR
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: April 17, 2018
#     Versión: 1.1
#
#     DESCRIPTION
#
#     Simulates a user defined number of samples and sample sized with user defined
#     distributions, expected values and variances. The distributions is the same in
#     every sample.
#
#     REQUIREMENT: requires getDist function 2.0, look for GetDist_20180417.R file
#
#     simSamples(n, mu, sigma, dist)
#
#     n: is a numerical vector of samples sizes.
#     mu: is a numerical vector of expected values.
#     sigma: is a numerical vector of standard deviations.
#     dist: is a vector of characters. Choose one type:
#         "Normal": Normal distribution.
#         "Normal2": Normal-squared distribution from a N(0,1). Basically a Chi^2 with df = 1.
#         "DoubleExp": Double exponential distribution (also known as Laplace distribution).
#         "DoubleExp2": Double exponential squared distribution from a DoubleExp(0,1).
#         "LogNormal": Lognormal distribution with logmu = 0 and logsigma = 1.
#         "Gamma": Gamma distribution
#         "Weibull": Weibull distribution
#     par.location: scalar, location parameter, by defaoult is 0**.
#     par.scale: scalar, scale parameter, by defaoult is 1**.
#     par.shape: scalar, shape parameter, by defaoult is 1.


simSamples <- function(n, mu, sigma, dist, par.location = 0, par.scale = 1, par.shape = 1){
  #Error verification
  #Error: number of mu and sigma elements does not match.
  if(length(mu) != length(sigma)){
    print("Error number of means and number of sigmas do not match")
    return()
  }
  if(!all(sigma > 0)){
    print("Error: negative sigma values")
    return()
  }
  feasibles = c("Normal", "Normal2", "DoubleExp", "DoubleExp2", "LogNormal", "Gamma", "Weibull")
  if(all(dist != feasibles)){
    print("Error: distribution not recognized")
    return()
  }
  
  #Actual simulation begins
  k = length(mu)
  N = sum(n)
  EX = rep(mu,n)
  sigmaX = rep(sigma,n)
  x = getDist(N, dist = dist, mu = EX, stdev = sigmaX, par.location = par.location, par.scale = par.scale, par.shape = par.shape)
  sampleID = factor(rep(1:k,n))
  xframe = data.frame(x,sampleID)
}

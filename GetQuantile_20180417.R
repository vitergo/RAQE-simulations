# #####################################################################################
#
#     QUANTILE CALCULATOR
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: April 17, 2018
#     Versión: BETA
#
#     DESCRIPTION
#
#     The quantile from several distributions is obtained
#
#     getQuantile(p, dist, par.location = 0, par.scale = 1, par.shape = 1)
#
#     p: is a scalar. Stands for the cumulative probability.
#     dist: is a vector of characters. Choose one type:
#         "Normal": Normal distribution.
#         "LogNormal": Lognormal distribution.
#         "Gamma": Gamma distribution
#         "Weibull": Weibull distribution
#     par.location: scalar, location parameter, by defaoult is 0**.
#     par.scale: scalar, scale parameter, by defaoult is 1**.
#     par.shape: scalar, shape parameter, by defaoult is 1.
#
#     **Note on par.location and par.scale:
#           par.location and par.scale are the location and scale parameters
#           of the distribution that generates the desired distribution. For lognormal,
#           par.location and par.scale correspond to the location and scale parameters of the normal
#           distribution that generales the lognormal. Hence, in this case they are the logmean and
#           the logsigma parameters. For Normal2 and DoubleExp2, par.location and par.scale correspond
#           correspond to the location and scale parameters of the normal and double exponential
#           that are used to generates their squared forms.


getQuantile <- function(p, dist, par.location = 0, par.scale = 1, par.shape = 1){
  if(dist == "Normal"){
    u = par.location
    s = par.scale
    
    EX = u
    VarX = s^s
    q = qnorm(p, mean = u, sd = s, lower.tail = TRUE, log.p = FALSE)
    q.z = (q - EX) / VarX^(0.5) #for dist centered on EX and VARX = 1
  }
  if(dist == "LogNormal"){
    u = par.location
    s = par.scale
    
    EX = exp(u + s^2/2)
    VarX = exp(2*(u + s^2)) - exp(2*u + s^2)
    q = qlnorm(p, meanlog = u, sdlog = s, lower.tail = TRUE, log.p = FALSE)
    q.z = (q - EX) / VarX^(0.5) #for dist centered on EX and VARX = 1
  }
  if(dist == "Gamma"){
    k = par.scale
    o = par.shape
    
    EX = k*o
    VarX = o*k^2
    q = qgamma(p, shape = o, scale = k, lower.tail = TRUE, log.p = FALSE)
    q.z = (q - EX) / VarX^(0.5) #for dist centered on EX and VARX = 1
  }
  if(dist == "Weibull"){
    k = par.shape
    l = par.scale
    
    EX = l * gamma(1 + 1/k)
    VarX = l^2 * (gamma(1+2/k) - (gamma(1+1/k))^2)
    q = qweibull(p, shape = k, scale = l, lower.tail = TRUE, log.p = FALSE)
    q.z = (q - EX) / VarX^(0.5) #for dist centered on EX and VARX = 1
  }
  quantile.value = list(q = q, q.z = q.z)
  return(quantile.value)
}
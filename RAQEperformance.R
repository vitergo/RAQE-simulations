# Function that evaluates the performance of RAQE

RAQUEperformance <- function(p, upper.tail, tail.percentage, n, mu, sdev, distr, replicates, par.location = 0, par.scale = 1, par.shape = 1, use.weights){
  q = rep(NA,replicates)
  fitted.function = rep(NA,replicates)
  for (i in 1:replicates) {
    data = simSamples(n = n, mu = mu, sigma = sdev, dist = distr, par.location = par.location, par.scale = par.scale, par.shape = par.shape)
    results = RAQUE(data = data, p = p, upper.tail = upper.tail, tail.percentage = tail.percentage, plot.empirical = FALSE, use.weights = use.weights)
    q[i] = results$Xq[1]
    fitted.function[i] = results$fitted.function[1]
  }
  
  # Mean, bias and standard error
  true.quantile = getQuantile(p = p, dist = distr, par.location = par.location, par.scale = par.scale, par.shape = par.shape)
  q.mean = mean(q)
  q.bias = q.mean - true.quantile$q.z
  q.sd = sd(q)
  
  # Best fit functions
  fitted.function = factor(fitted.function)
  best.fit = tapply(q, fitted.function, length)
  
  # MAD
  q.MAD = mean(abs(q - true.quantile$q.z))
  
  # Performance metrics no MAD
  performance = list(q.true = true.quantile$q.z, q.mean = q.mean, q.bias = q.bias, q.sd = q.sd, q.MAD = q.MAD, best.fit = best.fit)
  
  return(performance)
}

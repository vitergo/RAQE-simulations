#RAQE function
#This is an early version of RAQE package

RAQE <- function(data, p, upper.tail = TRUE, tail.percentage = 25, plot.empirical = TRUE, use.weights = FALSE){
  #read sample statistics
  xbar = tapply(data$x, data$sampleID, mean)
  s = tapply(data$x, data$sampleID, sd)
  n = tapply(data$x, data$sampleID, length)
  N = sum(n)
  
  #standardize samples
  Xbar = rep(xbar,n)
  S = rep(s,n)
  z = (data$x - Xbar)/S
  
  #Expand empirical distribution
  o = order(z)
  z = z[o]
  
  p.up = rank(z)/N
  p.down = (rank(z)-1) / N
  p.mid = (p.up + p.down) / 2
  
  z.up = z[2:N]
  z.down = z[1:(N-1)]
  z.mid = (z.up + z.down) / 2
  
  Z = c(z,z.mid)
  P = c(p.mid, p.up[1:(N-1)])
  
  O = order(Z)
  Z = Z[O]
  P = P[O]
  
  #Define tail to fit
  if(upper.tail == TRUE){
    Z.tail = Z[Z>=quantile(z, probs = 1 - tail.percentage/100)]
    P.tail = P[Z>=quantile(z, probs = 1 - tail.percentage/100)]
    datos = data.frame(Z = Z.tail, P = P.tail) 
  }else{
    Z.tail = Z[Z<=quantile(z, probs = tail.percentage/100)]
    P.tail = P[Z<=quantile(z, probs = tail.percentage/100)]
    datos = data.frame(Z = Z.tail, P = P.tail)
  }
  
  
  if(use.weights != FALSE){
    W = 1 / (P.tail*(1-P.tail)) 
  } else{
    W = 1
  }
  
  q = rep(NA,5)
  SSE = rep(NA,5)
  fitted.function = c("Gumbel.max", "Gumbel.min", "Exponential", "Weibull", "Logit")
  #  Gumbel type 1 (max extreme value). Goes slower than exponential
  fitGumbel.max = optim(par = c(0, 1), SSEgumbel.max, data = datos, W = W)
  q[1] = gumbel.max.inv(p = p, u = fitGumbel.max$par[1], s = fitGumbel.max$par[2])
  SSE[1] = fitGumbel.max$value
  
  #  Gumbel type 2? (min extreme value). Goes faster than exponential
  fitGumbel.min = optim(par = c(0, 1), SSEgumbel.min, data = datos, W = W)
  q[2] = gumbel.min.inv(p = p, u = fitGumbel.min$par[1], s = fitGumbel.min$par[2])
  SSE[2] = fitGumbel.min$value
  
  #  Exponential. Goes equal to the exponential! :)
  fitExponential = optim(par = c(0,1), SSEexponential, data = datos, W = W)
  q[3] = exponential.inv(p = p, u = fitExponential$par[1], theta = fitExponential$par[2])
  SSE[3] = fitExponential$value
  
  #  Weibull. Goes faster, equal or slower than exponential
  fitWeibull = optim(par = c(min(Z),1,1), SSEweibull, data = datos, W = W)
  q[4] = weibull.inv(p = p, u = fitWeibull$par[1], a = fitWeibull$par[2], b = fitWeibull$par[3])
  SSE[4] = fitWeibull$value
  
  #  Logit. Similar to normal
  fitLogit = optim(par = c(0, (sd(Z)*sqrt(3)/pi)), SSElogit, data = datos, W = W)
  q[5] = logit.inv(p = p, u = fitLogit$par[1], s = fitLogit$par[2])
  SSE[5] = fitLogit$value
  
  SSEm = matrix(SSE, nrow=1,ncol=5)
  colnames(SSEm) <- c("Gumbel.max","Gumbel.min","Exponential","Weibull", "Logit")
  rownames(SSEm) <- c("SSEw")
  
  o1 = order(SSE)
  zq = q[o1[1]]
  
  Xq = xbar + s*zq
  
  results = list(Xq = Xq, fitted.function = fitted.function[o1[1]], SSEm)

  
    
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Weibull"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P, pch = 20, cex = 0.5, xlim = c(min.Xplot,max.Xplot))
    points(zq, p, pch = 1, col = "blue")
    lines(xline, weibull(xline, u = fitWeibull$par[1], a = fitWeibull$par[2], b = fitWeibull$par[3]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Gumbel.max"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P, pch = 20, cex = 0.5, xlim = c(min.Xplot,max.Xplot))
    points(zq, p, pch = 1, col = "blue")
    lines(xline, gumbel.max(xline, u = fitGumbel.max$par[1], s = fitGumbel.max$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Gumbel.min"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P, pch = 20, cex = 0.5, xlim = c(min.Xplot,max.Xplot))
    points(zq, p, pch = 1, col = "blue")
    lines(xline, gumbel.min(xline, u = fitGumbel.min$par[1], s = fitGumbel.min$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Exponential"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P, pch = 20, cex = 0.5, xlim = c(min.Xplot,max.Xplot))
    points(zq, p, pch = 1, col = "blue")
    lines(xline, exponential(xline, u = fitExponential$par[1], theta = fitExponential$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Logit"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P, pch = 20, cex = 0.5, xlim = c(min.Xplot,max.Xplot))
    points(zq, p, pch = 1, col = "blue")
    lines(xline, logit(xline, u = fitLogit$par[1], s = fitLogit$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
  }
  
  return(results)
}

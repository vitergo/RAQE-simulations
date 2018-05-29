#............................................................................
# Load Database with Simulation Results
setwd("~/Documents/R Works/RAQUE")
RAQUEsimDB <- read.csv("RAQUE_simResults_20180427.CSV")
View(RAQUEsimDB)

#............................................................................
# Data preparation
# Define Factors and Levels

# Weights
fweights <- RAQUEsimDB$weights
fweights <- factor(fweights, levels = c(TRUE,FALSE))
levels(fweights)  <- c("Yes","No") #change TRUE/FALSE to Yes/No

# Tail Percentage
ftails <- RAQUEsimDB$tail.percentage
ftails <- factor(ftails, levels = c(10,20,30,40))

# Sample size
samplesize <- RAQUEsimDB$n
samplesize <- factor(samplesize)

p <- RAQUEsimDB$p
p <- factor(p)

#............................................................................
# Overall Analysis: Weights vs No Weights
# Overall: MAD
RAQUE.MAD <- RAQUEsimDB$MAD

boxplot(RAQUE.MAD~fweights)
tapply(X = RAQUE.MAD,INDEX = fweights, FUN = mean)
tapply(X = RAQUE.MAD,INDEX = fweights, FUN = median)
tapply(X = RAQUE.MAD,INDEX = fweights, FUN = sd)

# Overall: bias
RAQUE.bias <- RAQUEsimDB$bias
boxplot(RAQUE.bias~fweights)
tapply(X = RAQUE.bias,INDEX = fweights, FUN = mean)
tapply(X = RAQUE.bias,INDEX = fweights, FUN = median)
tapply(X = RAQUE.bias,INDEX = fweights, FUN = sd)

# Overall: sd
RAQUE.sd <- RAQUEsimDB$sdQ
boxplot(RAQUE.sd~fweights)
tapply(X = RAQUE.sd,INDEX = fweights, FUN = mean)
tapply(X = RAQUE.sd,INDEX = fweights, FUN = median)
tapply(X = RAQUE.sd,INDEX = fweights, FUN = sd)

#............................................................................
# 1 Factor: Tail Percentage
# MAD
boxplot(RAQUE.MAD~ftails)
tapply(X = RAQUE.MAD,INDEX = ftails, FUN = mean)
tapply(X = RAQUE.MAD,INDEX = ftails, FUN = median)

boxplot(RAQUE.MAD~ftails * fweights)
tapply(X = RAQUE.MAD,INDEX = list(fweights, ftails), FUN = mean)
tapply(X = RAQUE.MAD,INDEX = list(ftails, fweights), FUN = median)

# Bias
boxplot(RAQUE.bias~ftails)
tapply(X = RAQUE.bias,INDEX = ftails, FUN = mean)
tapply(X = RAQUE.MAD,INDEX = ftails, FUN = median)

tapply(X = RAQUE.MAD,INDEX = list(fweights, ftails), FUN = mean)
tapply(X = RAQUE.MAD,INDEX = list(fweights, ftails), FUN = median)
tapply(X = RAQUE.MAD,INDEX = list(ftails, fweights), FUN = sd)

# Factor: Sample size
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights), FUN = mean)
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights), FUN = median)
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights), FUN = sd)

# Factor: Sample size and p
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights, p), FUN = mean)
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights, p), FUN = median)
tapply(X = RAQUE.MAD,INDEX = list(ftails, samplesize, fweights, p), FUN = sd)

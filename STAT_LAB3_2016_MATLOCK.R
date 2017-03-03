rm(list=ls())

ss <- seq(0,1,0.001)
a <- dbeta(ss, 1,1)
b <- dbeta(ss, 6,6)

#1.A
plot(a, col="red")
par(new=T)
plot(b, col="blue", axes=F)

#1.B
#Make posterior graphs for two experiments:

#One with 1 heads and 1 tail.
aa <- seq(0,1,0.001)
post.A1 <- dbeta(aa, 2+1, 2+1)
post.A2 <- dbeta(aa, 2+6, 2+6)

plot(post.A2, col="red")
par(new=T)
plot(post.A1, col="blue", axes=F)

#One with 400 heads and 400 tails.
bb <- seq(0,1,0.001)
post.B1 <- dbeta(bb, 401+1, 401+1)
post.B2 <- dbeta(bb, 401+6, 401+6)

plot(post.B2, col="red")
par(new=T)
plot(post.B1, col="blue", axes=F)

# Why are the two posterior plots involving the 800 coin flips so similar?
# As n gets bigger the width of the beta distribution decreases.

# Why are the two posterior plots involving the 2 coin flips so different?
# Small values for shape parameters in the Beta distribution cause the graph to
# converge towards the Bernoulli distribution as the shape parameters 
# get close to 0.

#2.A
x <- seq(0,1,.001)
p <- dexp(x, rate =5)
plot(aa, dbeta(p,1,1))

#2.B
# Calculate the posterior graph with both the Metropolis algorithm 
# and grid approximation for a case with 14 heads and 10 tails 
# (where x = prob(head)). Show the two methods roughly agree.  
# Compare this to a plot with a posterior for 
# 14 heads and 10 tails generated from a prior with beta(10,10).

# Metropolis 
piOld <- 0.5
numIter <- 10000
M.posteriorDist <- vector()
for(i in 1:numIter)
{
  #prior beta(10,10)
  pOld <- dbeta(piOld,10,10)*dbinom(14,24, piOld)
  
  piNew <- piOld + rnorm(1,0,sd=0.01)
  if( piNew > 1)
    piNew = 1
  if(piNew < 0)
    piNew = 0  
  
  pNew <- dbeta( piNew, 10,10)*dbinom(14,24,piNew)
  ratio <- pNew/pOld

  if(ratio > 1 || ratio >= runif(1))
    piOld = piNew
  
  M.posteriorDist[i] = piOld
}

plot(M.posteriorDist, col="red")

# Grid Approximation

g.numIter <- 10000
G.posteriorDist <- vector()
xVals <- seq(0,1,1/g.numIter)

i <- 1
sum <- 0
for (x in xVals)
{
  G.posteriorDist[i] <- dbeta(x,10,10) * dbinom(14,24,x)
  sum = sum + G.posteriorDist[i]
  i <- i + 1
}
par(new=TRUE)
plot(G.posteriorDist/sum, axes=FALSE, col="steelblue")
#lines(dbeta(xVals, 10+14, 10+10)/g.numIter,col="green")

#2.C
# Metropolis 
piOld <- 0.5
numIter <- 10000
M.posteriorDist <- vector()
for(i in 1:numIter)
{
  #prior beta(10,10)
  pOld <- dbeta(piOld,10,10)*dbinom(583,1000, piOld)
  
  piNew <- piOld + rnorm(1,0,sd=0.0001)
  if( piNew > 1)
    piNew = 1
  if(piNew < 0)
    piNew = 0  
  
  pNew <- dbeta( piNew, 10,10)*dbinom(583,1000,piNew)
  ratio <- pNew/pOld
  
  if(ratio > 1 || ratio >= runif(1))
    piOld = piNew
  
  M.posteriorDist[i] = piOld
}

plot(M.posteriorDist, col="red")

# Grid Approximation

g.numIter <- 10000
G.posteriorDist <- vector()
xVals <- seq(0,1,1/g.numIter)

i <- 1
sum <- 0
for (x in xVals)
{
  G.posteriorDist[i] <- dbeta(x,10,10) * dbinom(583,1000,x)
  sum = sum + G.posteriorDist[i]
  i <- i + 1
}
par(new=TRUE)
plot(G.posteriorDist/sum, axes=FALSE, col="steelblue")
#lines(dbeta(xVals, 10+14, 10+10)/g.numIter,col="green")

#How do the three posterior curves relate to each other now?

#Why does this plot look different than the plot in (2B)?
#This plot looks like it has more noise than the plot in 2B.


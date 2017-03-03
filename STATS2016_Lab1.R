rm(list=ls())

# ******************LAB1 PART1********************* #

myRolls <- function(nRolls)
{
    all = c()
    #random <- runif(nRolls, 0, 1)
    for(i in 1:nRolls)
    {
  
      if(runif(1) < 0.5)
      {
        side <- sample(1:5, size=1)
        all[i] <- side
      }
      else
      {
        side <- 6
        all[i] <- side
      }
      
    }

  return(all)
  
}

dishonest.rolls <- myRolls(6)
dishonest.rolls
m.dis <- mean(dishonest.rolls)
m.dis
v.dis <- var(dishonest.rolls)
v.dis
# Does not follow uniform distribution. Would follow if all sides were equally likely
hist(dishonest.rolls)


# ******************LAB1 PART2********************* #

#rm(list=ls())
trialSizes <- c(5,10,15,20,25,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,10000,20000,30000,10000)
means <- vector(mode="double", length=length(trialSizes))
variances <- vector(mode="double", length=length(trialSizes))

for(i in 1:length(trialSizes))
{
  rolls <- vector(length=trialSizes[i], mode="double")
  
  for(j in 1:trialSizes[i])
  {
    rolls[j] <- myRolls(1)
  }
  means[i] <- mean(rolls)
  variances[i] <- var(rolls)

}

ex.mean <- sum(1*.1 + 2*.1 + 3*.1 + 4*.1 + 5*.1 + 6*.5)
ex.var <- sum((1^2)*.1 + (2^2)*.1 + (3^2)*.1 + (4^2)*.1 + (5^2)*.1 + (6^2)*.5)-(ex.mean^2)

plot(log10(trialSizes), means)
lines(log10(trialSizes), rep(ex.mean, length(trialSizes)))
quartz()
plot(log10(trialSizes), variances)
lines(log10(trialSizes), rep(ex.var, length(trialSizes)))






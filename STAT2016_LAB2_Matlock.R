#2.1
p <- (1/3)
n <- 30
k <- 12
choose(n,k)*p^k*(1-p)^(n-k)
seqs <- 1:n
length(seqs)
dist <- dbinom(seqs,k,p)
plot(seqs,dist, type="b")
#mean = np
n*p
#var = np(1-p)
n*p*(1-p)
#2.2
rm(list=ls())
sr <- 0.4
n <- 100
dead <- 47
live <- n-dead
#A
plot(1:live,dbinom(1:live,live,sr))
#B
pbinom(dead, n, 0.4)
#C
#if drug has no effect, death = "success" 
binom.test(dead,n,p=0.4, alternative = "less")$p.value
#2.3
#A
rm(list=ls())
samp <- rbinom(1000, 10000, 0.5)
head(samp)
length(samp)
str(samp)
#B
u <- mean(samp)
ex.u <- 10000*0.5
v <- var(samp)
ex.v <- (1/10000)*(10000-ex.u)^2
#C
ps <- c()
for(x in 1:length(samp))
{
  t <- binom.test(samp[x],10000,p=0.5, alternative = "greater")$p.value
  ps[x] <- t
}
hist(ps)
#expected uniform distribution and that is what you see 
#D
ts <- c()
for(x in 1:length(samp))
{
  t <- binom.test(samp[x],10000,p=0.2, alternative = "greater")$p.value
  ts[x] <- t
}
hist(ts)
# P-values all converge to 0
# For P-values 0.49 and 0.51 I would not expect the histogram to be the same as 
# it is when p=0.2 because since samp is randomly generated, it is likely that some 
# values are greater than 0.49 or 0.51. This can be seen when checking the confidence
# interval:
binom.test(5000,10000,0.5)




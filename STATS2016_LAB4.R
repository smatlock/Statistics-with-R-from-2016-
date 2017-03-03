rm(list=ls())
setwd("/Users/Smatlock/Stats2016")

myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)
c1 <- myT[,1]
c2 <- myT[,2]
plot(c1, c2, log="xy", col="steelblue3", main="log plot")

# Qualitatively, do the biological replicates appear to have similar patterns 
# of gene expression?

# Yes.

assigned_D201 <- myT[1,1]
assigned_D202 <- myT[1,2]
not_assigned_D201 <- length(c1)-1
not_assigned_D202 <- length(c2)-1
m <- matrix(c(assigned_D201,assigned_D202,not_assigned_D201, not_assigned_D202), 
            nrow=2, byrow=T,)
colnames(m) <- c("Sequences in D2_01", "Sequences in D2_02")
rownames(m) <- c("Assigned to NC101_00003", "Not assigned to NC101_00003")

#Contingency Table
print(m)
#Generate P-value:
p <- fisher.test(m,alternative = "two.sided" )$p.value

#Now generate a p-value for all the genes in the spreadsheet from the Fisher test.  
ps <- vector()
for(i in 1:length(c1))
{
  mat <- matrix(c(c1[i],c2[i],4630,4630), nrow=2, byrow=T)
  p.val <- fisher.test(mat,alternative = "two.sided" )$p.value
  ps[i] <- p.val
}

#Plot out those p-values in a histogram
hist(ps)

# Are they uniformly distributed?  
# No
# Would you expect them to be?  
# No
# Are the p-values more significant,less significant or what we would expect under a uniform distribution?  
# They are more significnt than expected under a uniform distribution

# Remove low abundance
new.myT <- myT[ (myT$D2_01 + myT$D2_02 > 50),]
new.c1 <- new.myT[,1]
new.c2 <- new.myT[,2]
ps <- vector()
for(i in 1:length(new.c1))
{
  mat <- matrix(c(new.c1[i],new.c2[i],4630,4630), nrow=2, byrow=T)
  p.val <- fisher.test(mat,alternative = "two.sided" )$p.value
  ps[i] <- p.val
}

#Plot out those p-values in a histogram
hist(ps, main="histogram of p-vals")

# How does the p-value distribution change if you remove low abundance genes 
# The p-value becomes much smaller, thus the data becomes more significant. 

#5 
#Pseudo-count
pseudo <- myT + 1

# expected frequency = p = Assigned to NC101_00003 in D2_01)/total # of sequences in D2_01.
expected.freq <- pseudo[1,1]/length(pseudo[,1])

#p value
p.val <- poisson.test(pseudo[1,2],length(pseudo[,1]),r=expected.freq)$p.value

#6
#repeat for every gene in spreadsheet

pseudo.p.vals <- vector()
for(i in 1:length(pseudo[,1]))
{
  e.freq1 <- pseudo[i,1]/length(pseudo[,1])
  p.val1 <- poisson.test(pseudo[i,2],length(pseudo[,1]),r=e.freq1)$p.value
  pseudo.p.vals[i] <- p.val1
}

hist(pseudo.p.vals, col="red")

# The histogram of p-vals from the Poisson test + pseudo-count 
# approximately agree with the values from the Fisher test.

#7. The pseudo count was added in step 5 because when doing a Poisson test
# lambda must be greater than zero. 



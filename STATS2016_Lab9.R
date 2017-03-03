rm(list=ls())

IN <- "/Users/Smatlock/Stats2016/prePostPhylum.txt"
data <- read.table(IN,sep="\t",header=TRUE)
myPCA <- princomp(data[5:10], cor = TRUE)

# percent variance described by 1st 2 PCs
temp <- myPCA$sdev^2
percent <- sum(temp[1:2])/sum(temp)

# plot PC1 vs PC2
PC1 <- myPCA$scores[,1]
PC2 <- myPCA$scores[,2]
plot(PC1, PC2)

par(mfrow=c(1,2))

#timepoint (PRE vs. POST) 

colors <- vector()
for(i in 1:length(data$time))
{
  if(data$time[i] == "POST")
    colors[i] = "BLUE"
  if(data$time[i] == "PRE")
    colors[i] = "RED"
}
  
plot(PC1, PC2, col=colors, main="Pre vs. Post Time", pch=16)
legend("bottomleft", c("pre","post"), col=c("red", "blue"), pch=16)

#agenotype ( WT vs. IL10-/-)
colors <- vector()
for(i in 1:length(data$genotype))
{
  if(data$genotype[i] == "WT")
    colors[i] = "GREEN"
  if(data$genotype[i] == "10-/-")
    colors[i] = "ORANGE"
}

plot(PC1, PC2, col=colors, main="WT vs. 10-/- Genotype", pch=16)
legend("bottomleft", c("WT","10-/-"), col=c("green", "orange"), pch=16)

#For each of the first two PCA axes, build a mixed linear model with 
#fixed variables of genotype and timepoint (PRE vs. POST) 
#and a random variable of cage.  

#PC1
library("nlme")
geno1.gls <- gls( PC1 ~ genotype + time , method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=data)
no_cage1.gls <- gls( PC1 ~ genotype + time , method = "REML",data=data)
anova(no_cage1.gls, geno1.gls)

#PC2
geno2.gls <- gls( PC2 ~ genotype + time, method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=data)
no_cage2.gls <- gls( PC2 ~ genotype + time, method = "REML",data=data)
anova(no_cage2.gls,geno2.gls)






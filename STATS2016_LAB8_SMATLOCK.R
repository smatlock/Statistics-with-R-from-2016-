## Shelby Matlock
## Lab 8
## 03/30/2016

rm(list=ls())

IN <- "/Users/Smatlock/Stats2016/prePostPhylum.txt"
data <- read.table(IN,sep="\t",header=TRUE)

# For the POST timepoints only:
# (1)  For each phyla, graph the relative abundance of that phyla vs. cage.  
# Does there appear to be a cage effect across different phyla?  

post_data <- data[data$time %in% c("POST"),]

# Graphing each of the phyla against cages

# Write Tenericutes graph to file
png("Tenericutes.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Tenericutes, 
     main="Distribution of Tenericutes Across Cages",
     xlab="Cages",
     ylab="Tenericutes Abundance")
dev.off()

# Write Verrucomicrobia graph to file
png("Verrucomicrobia.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Verrucomicrobia, 
     main="Distribution of Verrucomicrobia Across Cages",
     xlab="Cages",
     ylab="Verrucomicrobia Abundance",
     ylim=c(0,5.1))
dev.off()

# Write Bacteroidetes graph to file
png("Bacteroidetes.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Bacteroidetes, 
     main="Distribution of Bacteroidetes Across Cages",
     xlab="Cages",
     ylab="Bacteroidetes Abundance",
     ylim=c(4,5.5))
dev.off()

# Write Actinobacteria graph to file
png("Actinobacteria.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Actinobacteria, 
     main="Distribution of Actinobacteria Across Cages",
     xlab="Cages",
     ylab="Actinobacteria Abundance",
     ylim=c(2,3.33))
dev.off()

# Write Firmicutes graph to file
png("Firmicutes.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Firmicutes, 
     main="Distribution of Firmicutes Across Cages",
     xlab="Cages",
     ylab="Firmicutes Abundance",
     ylim=c(5.2,5.7))
dev.off()

# Write Proteobacteria graph to file
png("Proteobacteria.png", width=15, height=7, units="in", res=300)
plot(post_data$cage, post_data$Proteobacteria, 
     main="Distribution of Proteobacteria Across Cages",
     xlab="Cages",
     ylab="Proteobacteria Abundance",
     ylim=c(0.5,3.5))
dev.off()

library("nlme")

j=1
rhos <- vector()
pvals <- vector()
for(i in 5:10)
{
  bug <- post_data[,i]
  cage <- post_data$cage
  genotype <- post_data$genotype

  myF <- data.frame(bug,cage,genotype)
  plot(  myF$bug ~ myF$cage)
  stripchart(bug ~ cage, data = myF,vertical = TRUE, pch = 21, add=TRUE)
  M.gls <- gls( bug ~ genotype , method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myF)
  myLm <- gls( bug~ genotype, method = "REML",data=myF)
  nullLogLike = unclass(logLik(myLm))[1] 
  altLogLike = unclass(logLik(M.gls))[1]
  val <- -2 * nullLogLike + 2 * altLogLike
  ans <- 1-pchisq(val,1)
  pvals[j] <- ans
  #test for false discovery rate:
  print(sum(ans < 0.1))
  #intraclass corr coef: 
  rho <- coef(M.gls$modelStruct[1]$corStruct,unconstrained=FALSE)[[1]]
  rhos[j] <- rho
  i = i + 1
  j = j + 1
}




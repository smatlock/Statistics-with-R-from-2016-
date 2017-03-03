rm(list=ls())
setwd("/Users/Smatlock/Stats2016/longitdunalRNASeqData")
myT <- read.table("nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
numCols <- ncol(myT)
myColClasses <- c("character", rep("numeric", numCols))
myTAsNum <-read.table("nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,colClasses=myColClasses)

#Normalize
myTNorm <- myTAsNum

for ( i in 2:ncol(myTAsNum))
{
  colSum = sum(myTNorm[,i])
  myTNorm[,i] = myTNorm[,i]/colSum
}

#T-test
# columns 2:4 are the 2 week time point, 
# 5:7 are the 12 week timepoint, 
# 8:12 are the 20 week 
# 2 weeks vs. 12 weeks; 2 weeks vs. 20 weeks; 12 weeks vs. 20 weeks
week2_week20 <- c()
week2_week12 <- c()
week12_week20 <- c()

for( i in 1:length(myTNorm[,1]))
{
a <- t.test( myTNorm[ i,2:4 ], myTNorm[ i, 8:12] )$p.value
week2_week20[i] <- a
b <- t.test( myTNorm[ i,2:4 ], myTNorm[ i, 5:7] )$p.value
week2_week12[i] <- b
c <- t.test( myTNorm[ i,5:7 ], myTNorm[ i, 8:12] )$p.value
week12_week20[i] <- c
}

hist(week2_week20)
hist(week2_week12)
hist(week12_week20)

#week 2 and week 20 have the biggest significant differences.

#Adjust hypothesis
a.a <- p.adjust( week2_week12[!is.nan(week2_week12)], method="BH", n=length(week2_week12))
b.a <- p.adjust( week2_week20[!is.nan(week2_week20)], method="BH", n=length(week2_week20))
c.a <- p.adjust( week12_week20[!is.nan(week12_week20)], method="BH", n=length(week12_week20))

# At a 10% false discovery rate, 
# how many genes are significant for the three comparisons under the t-test?
sum(a.a < 0.1)
# 0 
sum(b.a < 0.1)
# 626
sum(c.a < 0.1)
# 37

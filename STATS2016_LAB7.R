## Shelby Matlock
## Lab 7
## 03/23/2016

IN <- "/Users/Smatlock/Stats2016/qPCRWithSampleDays.txt"
data <- read.table(IN,sep="\t",header=TRUE,row.names=1)
log16s <- data[,10]
sample_days <- data[,2]
plot(sample_days, log16s, main="Days vs. Log16S")

t_status <- data[,5]
colors <- vector()

for(i in 1:length(t_status))
{
  if(t_status[i] == "Treatment")
    colors[i] = "BLUE"
  
  if(t_status[i] == "Recovery")
    colors[i] = "GREEN"
  
  if(t_status[i] == "Before Treament")
    colors[i] = "RED"
  
  if(t_status[i] == "stable")
    colors[i] = "BLACK"
}
#1
plot(sample_days, log16s,col=colors, xlim=c(0,1200), ylim=c(6,11), pch=16, main="Status")
legend("bottomright", c("Treatment", "Recovery", "Before Treatment", "Stable"),
       col=c("blue", "red", "green", "black"), pch=16)

#2

#8 parameter model
treat <- factor(t_status)
treat <- relevel(treat, ref="Treatment")
full <- lm(log16s ~ sample_days * treat, x=TRUE)
full$x

#5 parameter model
reduced <- lm(log16s ~ sample_days + treat, x=TRUE)
reduced$x

#2 parameter model
plot(sample_days, log16s, main="Minimal")
minimal <- lm(log16s ~ sample_days)
abline(minimal)

fullResiduals <- sum(residuals(full)^2)
# 31.80586 error
# one slope, 4 intercepts
reducedResiduals <- sum(residuals(reduced)^2)
# 33.79789 error
# one slope, one intercept but fewer degrees of freedom
minResiduals <- sum(residuals(minimal)^2)
# 38.2617 error

# can check with anova
anova(full)
anova(reduced)
anova(minimal)

# model visualization
coefs <- coef(full)

plot(sample_days, log16s,col=colors, xlim=c(0,1200), ylim=c(6,11), pch=16, main="Full")
abline(full, col="BLUE") # treatment line
abline( a=coefs[1] + coefs[3], b=coefs[2], col="RED") # before treatment
abline( a=coefs[1] + coefs[4], b=coefs[2], col="GREEN") # recovery 
abline( a=coefs[1] + coefs[5], b=coefs[2], col="BLACK") # stable

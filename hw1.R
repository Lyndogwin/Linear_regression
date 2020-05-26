# install.packages("faraway")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("Hmisc")
require(faraway)
require(ggplot2)
require(reshape2)
require(Hmisc)

# ---------teen gambling data-----------
data(teengamb, package="faraway")
head(teengamb)
dim(teengamb)
# teengamb$sex = factor(teengamb$sex)
# levels(teengamb$sex) <- c("male", "female")
summary(teengamb$sex)

names = names(teengamb)
par(mfrow=c(1, length(names)), pty="s")
for (x in names){ 
    try({
        hist(teengamb[,x], xlab=x, main="")
    })
}    
# ggplot(melt(teengamb),aes(x=value)) + geom_histogram() + facet_wrap(~variable)
# hist(teengamb[class == 'numeric'])

pairs(teengamb)
x = teengamb$income
y = teengamb$gamble

Sxx = sum((x-mean(x))^2)
Sxy = sum(y*(x-mean(x)))
beta1 =  Sxy/Sxx
beta1
beta0 = mean(y)-beta1*mean(x)
beta0

fit = lm(y~x)
dev.new()
plot(x, y, xlab="Income", ylab="Gamble",main="Fitted Model", cex=1.5)
abline(a=beta0, b=beta1, cex=50, col="red") 

summary(y~x)
length(y)

summary(fit)$sigma
expt = mean(x)

# --------prostate cancer data-----------
data(prostate, package="faraway")

y = prostate$lpsa
x = prostate$lcavol
plot(x, y, xlab="lcavol", ylab="lpsa",main="Fitted Model", cex=1.5)
fit = lm(y~x)
abline(fit, cex=1.5, col="red")
sigma = summary(fit)$sigma
variance = sigma^2
variance

# --------problem 4 data----------------
x = c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3)
y = c(0.45, 0.2, 0.34, 0.58, 0.7, 0.57, 0.55, 0.44)

Sxx = sum((x-mean(x))^2)
Sxy = sum(y*(x-mean(x)))
beta1 =  Sxy/Sxx
beta1
beta0 = mean(y)-beta1*mean(x)
beta0


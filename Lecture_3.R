## ----tab1, include=TRUE, echo=FALSE, tab.cap="Rocket Propellent Data Set"-----------------------------------------------
y <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
x <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)
rocket.data <- cbind(Shear_Strength=y, Time=x)
knitr::kable(rocket.data)


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
y <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
x <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)
fit <- lm(y~x)
names(fit)
ei <- fit$residuals
y_hat <- fit$fitted.values
sum(ei)
sum(x*ei)
sum(y_hat*ei)
sum(y)
sum(y_hat)


## ----fig1, include=TRUE, echo=TRUE, fig.cap="Density plot of a standard normal distribution", fig.pos="h"---------------
zz <- seq(-5, 5, 0.1)
dz <- dnorm(zz)
plot(zz, dz, type="l", xlab="z", ylab="density", 
		main="Standard Normal Distribution")
abline(v=qnorm(0.95), col="red")
abline(v=qnorm(0.05), col="red")


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
sigma2 <- 9200
beta1_hat <- fit$coefficients[2]
beta1 <- 0 # beta1 is zero here becuase the null hypothesis is beta1 = 0
Sxx <- sum((x-mean(x))^2)
Z0 <- (beta1_hat-beta1)/sqrt(sigma2/Sxx) 
pvalue <- 2*pnorm(abs(Z0), lower.tail=FALSE)
Z0
pvalue


## ----fig2, include=TRUE, echo=TRUE, fig.cap="Density plot of t distribution (df=20)"------------------------------------
zz <- seq(-5, 5, 0.1)
dz <- dt(zz, df=20)
plot(zz, dz, type="l", xlab="t", ylab="density", 
		main="t-distribution with 20 degrees of freedom")
abline(v=qt(0.95, df=20), col="red")
abline(v=qt(0.05, df=20), col="red")


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
beta1 <- fit$coefficients[2]
Sxx <- sum((x-mean(x))^2)
n <- length(y)
MS <- sum(fit$residuals^2)/(n-2) # mean squared residuals
t0 <- (beta1-0)/sqrt(MS/Sxx)
pvalue.t <- 2*pt(abs(t0), df=(n-2), lower.tail=FALSE)
t0
pvalue.t
	
summary(fit)
#---------
##$ h0: beta1 = -20 vs H1: beta1 < -20
beta1_hat = fit$coefficients[2]
MS =  summary(fit)$sigma^2
Sxx = sum((x-mean(x))^2)
t0 = (beta1_hat-(-20))/sqrt(MS/Sxx) # standard error = sqrt(MS/Sxx)

p.value = pt(t0, df=18)
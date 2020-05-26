
# -------------rocket propellant data---------------- 
y <-c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575,2357.9, 
    2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30,2053.50, 2414.4, 
    2200.50, 2654.20, 1753.70)
x <-c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75,25, 9.75, 22, 
    18, 6, 12.5, 2, 21.5)
plot(x, y, xlab="Age of propellant (weeks)", ylab="Shear strength",main="Fitted Model", cex=1.5)

Sxx = sum((x-mean(x))^2)
Sxy = sum(y*(x-mean(x)))
beta1 =  Sxy/Sxx
beta1
beta0 = mean(y)-beta1*mean(x)
beta0
abline(a=beta0, b=beta1, cex=50, col="red") # add line to an existing plot


# linear model
fit = lm(y~x) # lm will create a simple linear model for the variables
summary(fit)
names(fit)

abline(lm(y~x), cex=1.5, col="blue") # fit exactly ontop of the line above

# estimate sigma^2
n = length(y)
e = fit$residuals
SS_res = sum(e^2)
sigma2_hat = SS_res/(n-2)
sigma = sqrt(sigma2_hat)

# or call from the model directly
sigma  = summary(fit)$sigma
sigma2_hat = sigma^2

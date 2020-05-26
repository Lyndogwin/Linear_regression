## ----fig1, include=TRUE, echo=FALSE, fig.cap="Scatter diagram of shear strength versus propellant age", fig.pos='h', fig.width=7----
SS <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
Age <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)
par(mfrow=c(1, 2))
plot(Age, SS, xlab="Age of propellant (weeks)", ylab="Shear strength", main="Scatter Plot", cex=1.5)
plot(Age, SS, xlab="Age of propellant (weeks)", ylab="Shear strength", main="Fitted Regression Line", cex=1.5)
abline(lm(SS~Age), cex=1.5, col="red")
par(mfrow=c(1, 1))


## ---- include=TRUE, echo=FALSE------------------------------------------------------------------------------------------
    x <- c(95, 85, 80, 70, 60)
    y <- c(85, 95, 70, 65, 70)
    grade <- cbind(x, y, (x-mean(x)), y-mean(y), (x-mean(x))^2, (x-mean(x))*y)
    colnames(grade) <- c("x_i", "y_i", "(x_i-x_bar)", "(y_i-y_bar)", "(x_i-x_bar)^2", "(x_i-x_bar)y_i")
    grade.sum <- apply(grade, 2, sum)
    knitr::kable(grade, caption="A toy example")


## ----tab1, include=TRUE, echo=FALSE-------------------------------------------------------------------------------------
SS <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
Age <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)

rocket <- cbind(SS, Age)
colnames(rocket) <- c("Shear Strength (psi)", "Age of Propellant (weeks)")
knitr::kable(rocket, caption="Data for Example 1: Rocket ")


## ----fig2, include=TRUE, echo=FALSE, fig.cap="Scatter Plot"-------------------------------------------------------------
plot(Age, SS)


## ----fig3, include=TRUE, echo=TRUE--------------------------------------------------------------------------------------
y <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
x <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)
plot(x, y, xlab="Age of propellant (weeks)", ylab="Shear strength", 
	main="Fitted Model", cex=1.5)

         
Sxx <- sum((x-mean(x))^2)
Sxy <- sum(y*(x-mean(x)))

beta1 <- Sxy/Sxx
beta1
beta0 <- mean(y)-beta1*mean(x)
beta0
abline(a=beta0, b=beta1, col="red")


## ----fig4, include=TRUE, echo=TRUE, fig.cap="Scatter plot with fitted regression model"---------------------------------
y <- c(2158.7, 1678.15, 2316, 2061.30, 2207.50, 1708.3, 1784.7, 2575, 
        2357.9, 2256.7, 2165.2, 2399.55, 1779.80, 2336.75, 1765.30, 
        2053.50, 2414.4, 2200.50, 2654.20, 1753.70)
x <- c(15.50, 23.75, 8, 17.00, 5.5, 19, 24, 2.5, 7.5, 11, 13, 3.75, 
         25, 9.75, 22, 18, 6, 12.5, 2, 21.5)
fit = lm(y~x)

summary(fit)
plot(x, y, xlab="Age of propellant (weeks)", ylab="Shear strength", 
	main="Fitted Model", cex=1)
abline(lm(y~x), cex=1, col="red")


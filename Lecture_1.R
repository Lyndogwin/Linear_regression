## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
# install.packages("faraway")
require(faraway)
data(pima, package="faraway")
head(pima)
dim(pima)


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
summary(pima)[, 1:4]
sort(pima$diastolic)[1:30]
sort(pima$glucose)[1:20]


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
summary(pima$test)
pima$test <- factor(pima$test)
levels(pima$test) <- c("negative", "positive")
summary(pima$test)
summary(pima)[, 1:4]

pima$diastolic[pima$diastolic==0] <- NA
pima$glucose[pima$glucose==0] <- NA
pima$triceps[pima$triceps==0] <- NA
pima$insulin[pima$insulin==0] <- NA
pima$bmi[pima$bmi==0] <- NA
head(pima, n=10)


## ----fig1, fig.cap="The first panel shows a histogram of the diastolic blood pressures, the second shows a kernel density estimate of the same, while the third shows an index plot of the sorted values", include=TRUE, echo=TRUE, fig.align = 'center', out.width='80%'----
par(mfrow=c(1, 3), pty="s")
hist(pima$diastolic, xlab="Diastolic", main="")
plot(density(pima$diastolic, na.rm=TRUE), main="")
plot(sort(pima$diastolic), ylab="Sorted Diastolic")


## ----fig2, include=TRUE, echo=TRUE, fig.cap="The left panel shows scatterplot of te diastolic blood pressures against diabetes function and the right panel shows boxplots of diabetes function broken down by test result", fig.pos = 'h'----
par(mfrow=c(1, 2), pty="s")
plot(diabetes~diastolic, data=pima)
plot(diabetes~test, data=pima)

pairs(pima[,1:8]) # all pair-wise scatter plots for variables 1 through 8


## ----fig3, include=TRUE, echo=TRUE, fig.cap="Histogram of diastolic generated by using ggplot function in package ggplot2"----
# install.packages("ggplot2")
require(ggplot2)
ggplot(na.omit(pima), aes(x=diastolic))+geom_histogram(binwidth=8)

## ----fig4, include=TRUE, echo=TRUE, fig.cap="Density plot generated by using ggplot function in package ggplot2"--------
ggplot(na.omit(pima), aes(x=diastolic))+geom_density()


## ----fig5, include=TRUE, echo=TRUE, fig.cap="Scatter plot generated by using ggplot function in package ggplot2"--------
ggplot(na.omit(pima), aes(x=diastolic, y=diabetes))+geom_point()


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
DT <- c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8, 17.83, 79.24, 
        21.50, 40.33, 21, 13.5, 19.75, 24, 29, 15.35, 19, 9.5, 
        35.1, 17.9, 52.32, 18.75, 19.83, 10.75)
NC <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
summary(DT)
summary(NC)


## ----fig6, include=TRUE, echo=TRUE, fig.cap="Scatter plot"--------------------------------------------------------------
par(mfrow=c(1, 2), pty="s")
plot(NC, DT, xlab="Delivery Volume, x", ylab="Delivery Time, y", 
     main="(a)")
plot(NC, DT, xlab="Delivery Volume, x", ylab="Delivery Time, y", 
     main="(b)")
abline(lm(DT~NC), cex=1.5, col="red")
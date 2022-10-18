# MLE Adelie Bill Length
a_bl = rnorm(333, 38.70, 2.58)
A_BLm <- mle2(a_bl~dnorm(mean=mu,sd=sd), start=list(mu=38.70,sd=2.58),data=data.frame(a_bl))
A_BLm

confint(A_BLm) # Confidence interval

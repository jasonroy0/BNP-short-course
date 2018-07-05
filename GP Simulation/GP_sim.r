library(tgp)
set.seed(8)

x <- seq(0, 1, .005)
y <- exp(4*x) + rnorm(length(x), mean = 0, sd = 3 )

i_miss <- x < .5
miss <- rbinom(n = length(x), size = 1, prob =  i_miss*.98 + (1 - i_miss)*.20)

x <- x[miss==0]
y <- y[miss==0]

gp <- bgp(X=x, XX=seq(0,1,.005), Z=y, meanfn = 'linear', corr='exp', bprior = 'bmzt')

par(mfrow=c(1,2))
plot(x, y, ylim=c(-20,60),pch=20,cex=1.2)
lines(seq(0,1,.005), gp$ZZ.mean, col='red', lwd=3)
lines(seq(0,1,.005), gp$ZZ.q1, col='red', lwd=3)
lines(seq(0,1,.005), gp$ZZ.q2 , col='red', lwd=3)
plot(function(x) exp(4*x), add=T, col='blue', lwd=2)


xs <- sample(1:length(x), size = 10, replace = F)
xx <- x[xs]
yy <- y[xs]

gp <- bgp(X=xx, XX=seq(0,1,.005), Z=yy, meanfn = 'linear', corr='exp', bprior = 'bmzt')

plot(xx, yy, ylim=c(-20,60),pch=20,cex=1.2, xlab='x', ylab='y')
lines(seq(0,1,.005), gp$ZZ.mean, col='red', lwd=3)
lines(seq(0,1,.005), gp$ZZ.q1, col='red', lwd=3)
lines(seq(0,1,.005), gp$ZZ.q2 , col='red', lwd=3)
plot(function(x) exp(4*x), add=T, col='blue', lwd=2)

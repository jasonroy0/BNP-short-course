# function for drawing from DP using stick breaking
rdp_stickbreak<-function(alpha, G_0){
  n <- 10000
  v <- rbeta(n = n, shape1 = 1, shape2 = alpha)
  
  w <- numeric(length = n)
  w[1] <- v[1]
  for(i in 2:n){
    w[i] <- v[i]*prod(1-v[1:(i-1)]) 
  }
  
  m <- G_0(n)
  draw_dp<-cbind(draw_val=m, draw_p=w)
  draw_dp<-draw_dp[order(draw_dp[,1]), ]
  
  return(draw_dp)
}


# draw from DP with standard normal base measure
draws <- replicate(15, rdp_stickbreak(200, G_0 = function(n) rnorm(n)) )

plot( draws[,1,1], cumsum(draws[,2,1]), type='l')
for(i in 2:15){
  lines( draws[,1,i], cumsum(draws[,2,i]), type='l')
}
plot(function(x) pnorm(x), add=T, col='red', xlim=c(-3,3), lwd=3 )

## now consider count data from poisson distribution
n <-1000
alpha <- 20

d <- rpois(n, lambda = 20)
table(d)

pr <- alpha*dpois(x = 0:30, lambda = 5)

post <- (alpha/(alpha+n))*pr + (n/(alpha+n))*c(0,table(d), rep(0, 8))

hist(d, freq = T)
lines(0:30, 1000*dpois(x = 0:30, lambda = 5), type='l')
lines(0:30, post, type='l')

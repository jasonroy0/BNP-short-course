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


library(MCMCpack)
# draw from DP with standard normal base measure
set.seed(10)
n <- 30
true_lambda <- 20

alpha <- 10
pr_lambda <- 20

d <- rpois(n, lambda = true_lambda)
d <- factor(d, levels=0:(max(d)+2))
d <- d[d!=18] ## remove 18 for illustration.

freq_tab <- table(d)/n
plot(freq_tab)

d <- as.numeric(as.character(d))

x_range <- 0:(max(d)+2)
pr_mean <- dpois(x = x_range, lambda = pr_lambda)
names(pr_mean) <- x_range

post_exp <- (alpha/(alpha+n))*pr_mean + (n/(alpha+n))*freq_tab

# take some illustrative draws from posterior, also a DP.
tt<-rdirichlet(n = 100, alpha = alpha*pr_mean + n*freq_tab )

plot(freq_tab, xlim=c(0, max(d)+2), ylim=c(0,.25),
     main = 'Observed Data with Posterior Mean of Data Distribution',
     xlab='y', ylab='Probability/Relative Frequency of Observed Data')

for(i in 2:100){
  lines( x_range, tt[i,], type='l', col='gray')
}

lines(x_range, post_exp, type='l', col='red', lwd=3) 

lines(x_range, pr_mean, type='l', col='blue', lwd=3)

lines(freq_tab, type='h')

legend('topleft', 
       legend = c('Observed Frequency',
                  'Prior mean, E[DP(alpha, G_0)] = G_0',
                  'Posterior mean',
                  '100 Draws from Posterior'),
       col=c('black','blue','red','gray'),
       lty = c(1,1,1), lwd=c(1,1,3),
       bty='n')




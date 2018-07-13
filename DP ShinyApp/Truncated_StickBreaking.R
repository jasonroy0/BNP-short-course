set.seed(1)

# simulate data
n <- 500
class <- sample(x = 1:3, size = n, replace = T)
y <- rnorm(n = n,mean = ifelse(class==1, 5, ifelse(class==2, 25, 50)), 5)
hist(y, breaks=50)

# stick-breaking functions
stickbreak<-function(alpha, K){
  v <- rbeta(n = K, shape1 = 1, shape2 = alpha)
  
  w <- numeric(length = K)
  w[1] <- v[1]
  v[K] <- 1
  for(i in 2:K){
    w[i] <- v[i]*prod(1-v[1:(i-1)]) 
  }
  
  return(w)
}

# parameters
gibbs_iter <- 1000
burnin <- 500
K <- 10

lambda <- 50
tau <- 100
phi <- 25
alpha <- 1

# create shells to store gibbs draws
class_shell <- matrix(NA, nrow = n, ncol = gibbs_iter)
mu_shell <- matrix(NA, nrow = K, ncol = gibbs_iter)
w_shell <- matrix(NA, nrow = K, ncol=gibbs_iter)
v_shell <- numeric(length = K)
post_pred_shell <- matrix(NA, nrow = n, ncol = gibbs_iter)

## initialize values
w_shell[,1] <- stickbreak(alpha = alpha, K = K)
mu_shell[,1] <- rnorm(n = K, lambda, sqrt(tau) )


for(i in 2:gibbs_iter){
  
  ### update clusters
  for(j in 1:n){
    p_clus <- w_shell[,i-1] * dnorm(x = y[j], mean = mu_shell[,i-1], sd = sqrt(phi) )
    class_shell[j,i] <- sample(x = 1:K, size = 1, replace = T, prob = p_clus)
  }
  
  class <- class_shell[,i]
  
  ### update weights
  for(k in 1:(K-1) ){
    A_k <- sum(class==k)
    B_k <- sum(class>k)
    v_shell[k] <- rbeta(1, A_k + 1, B_k + alpha)
  }
  v_shell[K] <- 1
  
  w_shell[1,i] <- v_shell[1]
  for(k in 2:K){
    w_shell[k,i] <- v_shell[k]*prod(1-v_shell[1:(k-1)]) 
  }
  
  ### update mu
  for(k in 1:K){
    y_k <- y[class==k]
    n_k <- length(y_k)
    
    post_var <- 1/(1/tau + n_k/phi)
    post_mean <- post_var*(lambda/tau + sum(y_k)/(phi) )

    mu_shell[k, i] <- rnorm(n = 1, 
                            mean = post_mean, 
                            sd = sqrt( post_var ) )
    
  }
  
  ### posterior predictive draw
  post_pred_shell[,i] <- rnorm(n = n, mean = mu_shell[class,i], sd = sqrt(phi))
  
}

hist(y, freq=F)
for(i in burnin:gibbs_iter){  
  lines(density(post_pred_shell[,i])) 
}

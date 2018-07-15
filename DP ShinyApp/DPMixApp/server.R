
# conditionally install required packages if not already installed.
package_list <- c("MCMCpack")
not_intallated <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(not_intallated)>0) install.packages(not_intallated, dependencies = TRUE)

library(shiny)
library(MCMCpack)

###--------------------------------------------------------------------------###
####                        helper functions                                ####
###--------------------------------------------------------------------------###
rdp_stickbreak<-function(alpha, G_0){
  n <- 2500
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

parse_input_DPdraw <- function(base){
  if(base==1){
    G_0 = function(n) rnorm(n)
    cdf = function(x) pnorm(x)
    xlim = c(-4,4)
    type='l'
  }else if(base==2){
    G_0 = function(n) rbeta(n,shape1 = 2, shape2 = 2)
    cdf = function(x) pbeta(x,shape1 = 2, shape2 = 2)
    xlim = c(0,1)
    type='l'
  }else{
    G_0 = function(n) rpois(n, lambda = 10)
    cdf = function(x) ppois(x,lambda = 10)
    xlim = c(0,20)
    type='s'
  }
  return(list(G_0=G_0, cdf=cdf, xlim=xlim, type=type))
}


gibbs_samp <- function(gibbs_iter, lambda, tau, alpha){
  
  K <- 10
  phi <- 25
  
  # create shells to store gibbs draws
  class_shell <- matrix(NA, nrow = n, ncol = gibbs_iter)
  mu_shell <- matrix(NA, nrow = K, ncol = gibbs_iter)
  w_shell <- matrix(NA, nrow = K, ncol=gibbs_iter)
  v_shell <- numeric(length = K)
  post_pred_shell <- matrix(NA, nrow = n, ncol = gibbs_iter)
  
  ## initialize values
  w_shell[,1] <- stickbreak(alpha = alpha, K = K)
  mu_shell[,1] <- rep(50, K)
  
  set.seed(1)
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
      
      ## posterior mean/variance using conjugacy.
      post_var <- 1/(1/tau + n_k/phi)
      post_mean <- post_var*(lambda/tau + sum(y_k)/(phi) )
      
      mu_shell[k, i] <- rnorm(n = 1, 
                              mean = post_mean, 
                              sd = sqrt( post_var ) )
    
    }
    #if(i==3) browser()
    post_pred_shell[,i] <- rnorm(n = n, mean = mu_shell[class,i], sd = sqrt(phi))
  }
  return(list(post_pred_shell = post_pred_shell, 
              mu_shell = mu_shell, 
              class_shell = class_shell))
}
  

###------------------------------------------------------------------------###
####                        Shiny Server Code                             ####
###------------------------------------------------------------------------###
shinyServer(function(input, output) {

  ###------------------------------------------------------------------------###
  ####                  Panel 1a - Drawing from a DP                        ####
  ###------------------------------------------------------------------------###

  output$DPdraws <- renderPlot({
    
    parsed<-parse_input_DPdraw(input$base)
    
    draws <- replicate(10, rdp_stickbreak(input$alpha, G_0 = parsed$G_0 ) )
    
    plot( draws[,1,1], cumsum(draws[,2,1]), type=parsed$type, 
          ylab='CDFs Drawn from Dirichlet Process', xlab='x',
          main = 'Ten realizations of a Dirichlet process with given concentration and base distribution')
    for(i in 2:10){
      lines( draws[,1,i], cumsum(draws[,2,i]), type=parsed$type)
    }
    
    cdf <- parsed$cdf
    curve(expr = cdf, add=T, col='red', xlim= parsed$xlim, lwd=3, type=parsed$type)
    
    legend('bottomright', 
           legend = c('Draws from DP(alpha, G_0)', 'E[DP(alpha, G_0)] = G_0'),
           col=c('black','red'),
           lty = c(1,1), lwd=c(1,3),
           bty='n')
    
  })
  
  ###------------------------------------------------------------------------###
  ####                  Panel 1b - Stick Breaking                           ####
  ###------------------------------------------------------------------------###
  
  output$StickBreakP1 <- renderPlot({
    
    parsed<-parse_input_DPdraw(input$base)
    draws <- replicate(1, rdp_stickbreak(input$alpha, G_0 = parsed$G_0 ) )
    
    par(mfrow=c(1,2))
    plot(x = draws[,1,1], y=draws[,2,1], xlab='Atoms, m_i', ylab='weights, w_i',
         main = 'Realization of a stick-breaking process with 2500 breaks',
         type='h')
    plot(x = draws[order(draws[,1,1]),1,1], 
         y= cumsum(draws[order(draws[,1,1]),2,1]), xlab='Atoms, v', ylab='weights, w',
         main = 'Realization of a stick-breaking process with 2500 breaks',
         type='s')
    cdf <- parsed$cdf
    curve(expr = cdf, add=T, col='red', xlim= parsed$xlim, lwd=3, type=parsed$type)
    legend('bottomright', 
           legend = c('Draws from DP(alpha, G_0)', 'E[DP(alpha, G_0)] = G_0'),
           col=c('black','red'),
           lty = c(1,1), lwd=c(1,3),
           bty='n')
    })
  
  ###------------------------------------------------------------------------###
  ####                  Panel 2a - Posterior Example                        ####
  ###------------------------------------------------------------------------###
  
  set.seed(10)
  n <- 30
  true_lambda <- 20
  n_post_draws <- 100
  d <- rpois(n, lambda = true_lambda)
  
  d <- factor(d, levels=0:(max(d)+2))
  d <- d[d!=18] ## remove 18 for illustration.
  
  freq_tab <- table(d)/n
  
  
  output$PoissonPosterior <- renderPlot({
    
    # draw from DP with standard normal base measure
    set.seed(10)
    n <- 30
    true_lambda <- 20
    
    alpha <- input$alpha_2
    pr_lambda <- input$pr_lambda
    
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
    post_draws<-rdirichlet(n = n_post_draws, alpha = alpha*pr_mean + n*freq_tab)
    
    plot(freq_tab, xlim=c(0, max(d)+2), ylim=c(0,.25),
         main = 'Observed Data with Posterior Mean of Data Distribution',
         xlab='y', ylab='Probability/Relative Frequency of Observed Data')
    
    for(i in 2:n_post_draws){
      lines( x_range, post_draws[i,], type='l', col='gray')
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
           lty = c(1,1,1), lwd=c(3,3,3),
           bty='n')
  })
  
  ###------------------------------------------------------------------------###
  ####                  Panel 2b - Bayesian Bootstrap                       ####
  ###------------------------------------------------------------------------###
  
  #post_draws<-rdirichlet(n = n_post_draws, alpha = alpha*pr_mean + n*freq_tab)
  
  output$tab <- renderTable({
    set.seed(10)
    n <- 30
    true_lambda <- 20
    n_post_draws <- 100
    d <- rpois(n, lambda = true_lambda)
    
    d <- factor(d, levels=0:(max(d)+2))
    d <- d[d!=18] ## remove 18 for illustration.
    
    freq_tab <- matrix(table(as.numeric(as.character(d) ) ), nrow=1)
    colnames(freq_tab) <- names(table(as.numeric(as.character(d) )))
    freq_tab
  })
  
  output$bootPlot <- renderPlot({
    
    n_boot <- input$n_boot
    
    set.seed(10)
    
    n <- 30
    true_lambda <- 20
    d <- rpois(n, lambda = true_lambda)
    
    d <- factor(d, levels=0:(max(d)+2))
    d <- d[d!=18] ## remove 18 for illustration.
    
    freq_tab <- table(d)/n
    
    freq_non_zero <- freq_tab[freq_tab>0]
    post_draws <- rdirichlet(n = n_boot, alpha = n*freq_non_zero)

    ## bayesian bootstrap
    res_bayes <- apply(post_draws, 1, function(x)  sum(x*as.numeric(names(freq_non_zero))) )
    
    ## frequentist bootstrap
    res_freq <- replicate(n_boot, expr = { mean(sample(x = as.numeric(as.character(d) ), size = n, replace = T)) })
    
    
    par(mfrow=c(2,1))
    
    hist(res_bayes, breaks=50, xlim=c(17,23),
         main=paste0('Posterior Distribution of mean, \nBayesian Boostrap (B=',n_boot,")"),
         xlab='mean')
    abline(v=mean(res_bayes), col='blue', lwd=3)
    abline(v=20, col='red', lwd=3)
    legend('topleft', legend = c('True Lambda', 'Posterior Mean'), col=c('red','blue'), lwd=c(3,3), bty='n')
    
    hist(res_freq, breaks=50, xlim=c(17,23),
         main=paste0('Sampling Distribution of mean, \nClassical Boostrap (B=',n_boot,')'),
         xlab='mean')
    abline(v=mean(res_freq), col='blue', lwd=3)
    abline(v=20, col='red', lwd=3)
    legend('topleft', legend = c('True Lambda', 'Sampling Dist. Mean'), col=c('red','blue'), lwd=c(3,3), bty='n')
    
  })
  
  ###------------------------------------------------------------------------###
  ####                  Panel 3a - DP Mixture Distribution                  ####
  ###------------------------------------------------------------------------###
  
  set.seed(1)
  
  # simulate data
  n <- 500
  class <- sample(x = 1:3, size = n, replace = T)
  y <- rnorm(n = n,mean = ifelse(class==1, 5, ifelse(class==2, 25, 50)), 5)
  
  colfunc <- colorRampPalette(c("lightgray", "darkred"))
  col_vec <- colfunc(20)
  
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
  
  output$mixDensity <- renderPlot({
    
    res <- gibbs_samp(gibbs_iter = input$gibbs_iter, 
                      lambda = input$lambda, tau = input$tau , 
                      alpha = input$alpha_3 )
    
    mu_shell <- res$mu_shell
    class_shell <- res$class_shell
    post_pred <- res$post_pred_shell
    
    
    par(mfrow=c(1,2))
    
    for(i in 2:input$gibbs_iter){
      
      hist(y, xlim=c(0,60), freq = F, ylim=c(0,.05))
      lines(density(post_pred[,i]))
      
      
      plot(mu_shell[class_shell[1,i],2], type='l', ylim=c(-10,100), xlim=c(0,20),
           xlab='MCMC iteration', ylab='Gaussian Means',
           main = 'MCMC chains for Gaussian mixture Means')
      lines(mu_shell[class_shell[2,i], 2:i])
      lines(mu_shell[class_shell[10,i], 2:i])
      lines(mu_shell[class_shell[13,i], 2:i])
      
    }
    
  })
  
  output$mixDensity <- renderPlot({
    
    res <- gibbs_samp(gibbs_iter = input$gibbs_iter, 
                      lambda = input$lambda, tau = input$tau , 
                      alpha = input$alpha_3 )
    
    mu_shell <- res$mu_shell
    class_shell <- res$class_shell
    post_pred <- res$post_pred_shell
    
    
    par(mfrow=c(1,2))
    
    for(i in 2:input$gibbs_iter){
      
      hist(y, xlim=c(0,60), freq = F, ylim=c(0,.05))
      lines(density(post_pred[,i]))
      
      
      plot(mu_shell[class_shell[1,i],2], type='l', ylim=c(-10,100), xlim=c(0,20),
           xlab='MCMC iteration', ylab='Gaussian Means',
           main = 'MCMC chains for Gaussian mixture Means')
      for(j in 2:20){
        lines(mu_shell[class_shell[j,i], 2:i])
      }
    }
    
  })
  
  # output$mixClass <- renderPlot({
  #   
  #   res <- gibbs_samp(gibbs_iter = input$gibbs_iter, 
  #                     lambda = input$lambda, tau = input$tau , 
  #                     alpha = input$alpha_3 )
  #   
  #   res <- gibbs_samp(gibbs_iter = 1000,
  #                     lambda = 50, tau = 100 , 
  #                     alpha = 1 )
  #   
  #   mu_shell <- res$mu_shell
  #   class_shell <- res$class_shell
  #   post_pred <- res$post_pred_shell
  #   
  #   par(mfrow=c(1,1))
  #   
  #   for(i in 2:input$gibbs_iter){
  #     
  #     tab<-apply(class_shell[,2:i, drop=F], 1, function(x) sort(x,decreasing = T)[1] )
  #     tab <- table(factor(tab, levels = 1:K))/n
  #     plot(tab)
  #     
  #   }
  #   
  # })
  
})

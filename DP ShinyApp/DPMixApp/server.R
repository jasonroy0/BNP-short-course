library(shiny)
library(MCMCpack)

package_list <- c("MCMCpack",'pacman','rgdal')
not_intallated <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(not_intallated)>0) install.packages(not_intallated, dependencies = TRUE)

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
  d <- rpois(n, lambda = true_lambda)
  #output$freq_tab <- renderTable({table(d)})
  
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
  })
  
  ###------------------------------------------------------------------------###
  ####                  Panel 2b - Bayesian Bootstrap                       ####
  ###------------------------------------------------------------------------###
  
  
})

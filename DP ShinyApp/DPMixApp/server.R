library(shiny)

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
  # draw from DP with standard normal base measure

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
  
  output$StickBreakP1 <- renderPlot({
    
    parsed<-parse_input_DPdraw(input$base)
    draws <- replicate(1, rdp_stickbreak(input$alpha, G_0 = parsed$G_0 ) )
    
    plot(x = draws[,1,1], y=draws[,2,1], xlab='Atoms, v', ylab='weights, w',
         main = 'Realization of a stick-breaking process with 2500 breaks',
         type='h')
    
  })
  
})

library(shiny)
tt <- readChar('test.txt', nchars = file.info('test.txt')$size)
# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Interactive Dirichlet Process (DP) Tutorial",
    tabPanel("Realizations of a DP",
             sidebarLayout(
               sidebarPanel(
                 
                 radioButtons(inputId="base", 
                              label = "Base Distribution, \\(G_0\\):",
                              choices = list("Standard Normal" = 1, 
                                             "Beta(2,2)" = 2, 
                                             "Pois(10)" = 3), 
                              selected = 1),
                 
                 sliderInput(inputId = 'alpha',
                             label = "Concentration parameter, \\(\\alpha\\):",
                             min = 1,
                             max = 500,
                             value = 100)
               ),
               
               
               mainPanel(withMathJax(),
                         tabsetPanel(
                           tabPanel("Draws from a DP",
                                    HTML("<h1> What Do Draws from Dirichlet Process Look Like?</h1>A Dirichlet Process is a 'distribution over distributions'. It is characterized by two paramaters: 
                                  the base distribution, \\(G_0\\), and concentration parameter \\( \\alpha \\).",
                                         "<br> <br>",
                                         "A draw or realization, \\(G\\), from a \\(DP(\\alpha, G_0)\\) is itself a distribution. We denote a draw/realization by $$G \\sim DP(\\alpha, G_0)$$",
                                         "The draws are ceneterd around \\(G_0\\), in a sense the 'mean' distribution.",
                                         "<br> <br>",
                                         "High values of \\( \\alpha \\) yields draws that are more tightly distributed, or 'concentrated', about \\(G_0\\).",
                                         "Low values of \\( \\alpha \\) yields draws that are more widely distributed about \\(G_0\\)."),
                                    plotOutput("DPdraws"),
                                    HTML("Notice that the distributions drawn from a DP are discrete, as seen by the stepwise nature of the drawn CDFs.")),
                           tabPanel("Stick-Breaking")
                         )
               )
               )
             )  ,
    tabPanel("DP Posterior"),
    tabPanel("DP Mixtures")
                  
  
  # Sidebar with a slider input for number of bins 
  
  )
)

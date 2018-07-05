library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Interactive Dirichlet Process Tutorial",
    tabPanel("Draws from the Dirichlet Process",
             sidebarLayout(
               sidebarPanel(
                 
                 radioButtons(inputId="base", 
                              label = "Base Distribution (G_0)",
                              choices = list("Standard Normal" = 1, 
                                             "Beta(2,2)" = 2, 
                                             "Pois(10)" = 3), 
                              selected = 1),
                 
                 sliderInput(inputId = 'alpha',
                             label = "Concentration parameter (alpha):",
                             min = 1,
                             max = 500,
                             value = 100)
               ),
               
               
               mainPanel(plotOutput("DPdraws"))
               )
             )  ,
    tabPanel("C2"),
    tabPanel("C3")
                  
  
  # Sidebar with a slider input for number of bins 
  
  )
)

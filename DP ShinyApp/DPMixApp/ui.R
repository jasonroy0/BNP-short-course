library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Interactive Dirichlet Process Tutorial",
                   tabPanel('0. Introduction',
                            mainPanel(includeMarkdown('Introduction.md'))
                            ),
                   tabPanel("1. Realizations of a DP",
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
                                          
                                          
                                          tabPanel("1.a. Draws from a DP",
                                                   HTML("<h1> What Do Draws from Dirichlet Process Look Like?</h1>",
                                                        "A Dirichlet Process is a 'distribution over distributions'. It is characterized by two paramaters: 
                                                        the base distribution, \\(G_0\\), and concentration parameter \\( \\alpha \\).",
                                                        "<br> <br>",
                                                        "A draw or realization, \\(G\\), from a \\(DP(\\alpha, G_0)\\) is itself a distribution. We denote a draw/realization by $$G \\sim DP(\\alpha, G_0)$$",
                                                        "The draws are ceneterd around \\(G_0\\), in a sense the 'mean' distribution.",
                                                        "<br> <br>",
                                                        "High values of \\( \\alpha \\) yields draws that are more tightly distributed, or 'concentrated', about \\(G_0\\).",
                                                        "Low values of \\( \\alpha \\) yields draws that are more widely distributed about \\(G_0\\)."),
                                                   plotOutput("DPdraws"),
                                                   HTML("Notice that the distributions drawn from a DP are discrete, as seen by the stepwise nature of the drawn CDFs.")),
                                          
                                          
                                          tabPanel("1.b. Stick-Breaking", 
                                                   HTML("<h1> How to obtain draws from a DP </h1>",
                                                        "In the previous panel, we displayed draws from the Dirichlet Process but did not describe how they were obtained. In this panel, we'll describe one common characterization of the DP which also serves as a way to sample from the process.",
                                                        "The Dirichlet Process is a distribution, but an infinite dimensional one. As such, we cannot represent DP measures using density functions, as we would with many familiar measures on random variables.",
                                                        "Instead, we can only characterize the Dirichlet Process through constructive/generative algorithms that allow us to sample from it.",
                                                        "<br> <br>", 
                                                        "One such constructive algorithm for sampling from a DP is the so-called stick-breaking process.", 
                                                        "Recall that the two parameters of the DP are the concentration parameter, \\( \\alpha \\), and the base distribution, \\( G_0 \\).",
                                                        "The stick-breaking algorithm proceeds as follows.",
                                                        "<br> <br>",
                                                        "<ol>
                                                            <li> Draw a random variable  \\(v_1 \\sim Beta(1, \\alpha) \\). </li>
                                                            <li> Draw a random variable \\( m_1 \\sim G_0 \\).    </li>
                                                            <li> Define \\( w_1 := v_1 \\). Let \\(w_1\\) be the measure on \\(m_1\\). </li>
                                                            <li> Now, for \\( i=2,3,4 \\dots  \\), make the independent draws \\(v_i \\sim Beta(1, \\alpha) \\)  </li>
                                                            <li> Construct \\( w_i \\) as \\(w_i := v_i \\prod_{j < i}(1 - v_j) \\).  </li>
                                                            <li> Now, for \\( i=2,3,4 \\dots  \\), make the independent draws \\(m_i \\sim G_0 \\).  </li>
                                                            <li> Let \\( w_i \\) be the measure on \\( m_i \\).  </li>
                                                        </ol>",
                                                        "Note that theoretically we do this to infinity for \\(i=1,2, \\dots \\). The \\(m_i\\) are often called 'atoms' and the \\(w_i\\)$ are often referred to as 'weights'.",
                                                        "<br> <br>", 
                                                        "This process is referred to as stick-breaking because it can be seen as breaking a unit-length stick into successively smaller sticks. For example, \\( v_1=w_1 = .25 \\) is analgous to breaking off one-fourth of the unit stick we start with. If \\(v_2=.5\\), then \\(w_2 = .5 \\cdot (1-.25) \\). Going with the analogy, we're breaking off one-half of the three quarters of the stick remaining from the previous break." ),
                                                   plotOutput("StickBreakP1"))
                                          )
                                        )
                              )
                            ),
                   
                   tabPanel("2. DP Posterior"),
                   
                   tabPanel("3. DP Mixtures"),
                   
                   tabPanel("4. References")
                  
  )
)

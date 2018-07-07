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
                                             choices = list("N(0,1)" = 1, 
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
                                                            <li> Draw a random variable  \\(v_1 \\sim Beta(1, \\alpha) \\). Draw a random variable \\( m_1 \\sim G_0 \\). Define \\( w_1 := v_1 \\). </li>
                                                            <li> Now, for \\( i=2,3,4 \\dots  \\), make the independent draws \\(v_i \\sim Beta(1, \\alpha) \\). Construct \\( w_i \\) as \\(w_i := v_i \\prod_{j < i}(1 - v_j) \\).  </li>
                                                            <li> Now, for \\( i=2,3,4 \\dots  \\), make the independent draws \\(m_i \\sim G_0 \\).  </li>
                                                        </ol>",
                                                        "Note that theoretically we do this to infinity for \\(i=1,2, \\dots \\). The \\(m_i\\) are often called 'atoms' and the \\(w_i\\) are often referred to as 'weights'.",
                                                        "A draw \\( G(\\cdot) \\) from a \\( DP(\\alpha G_0) \\) is represented by",
                                                        "$$G(\\cdot) = \\sum_{i=1}^\\infty w_i \\delta_{m_i}(\\cdot)$$",
                                                        "Above, \\( \\delta_{m_i}(x) = I(x = m_i) \\) is an indicator of whether the input equals \\( m_i \\). If it does, we place the probability measure \\( w_i \\) on it.", 
                                                        "Because the atoms are drawn from the base distribution, draws from the DP have the same support as the base distribution. The measure is 0 for values that don't match any atoms.",
                                                        "<br> <br>", 
                                                        "This process is referred to as stick-breaking because it can be seen as breaking a unit-length stick into successively smaller sticks. For example, \\( v_1=w_1 = .25 \\) is analgous to breaking off one-fourth of the unit stick we start with. If \\(v_2=.5\\), then \\(w_2 = .5 \\cdot (1-.25) \\)." ,
                                                        "Going with the analogy, we're breaking off one-half of the three quarters of the stick remaining from the previous break.",
                                                        "<br> <br>",
                                                        "In practice we can't keep breaking the stick forever. The plots below demonstrate stick-breaking for 2500 breaks."),
                                                   plotOutput("StickBreakP1"),
                                                   HTML("The first plot places a vertical bar at each atom. The height of each bar corresponds to the weight associated with each atom.",
                                                        "This plot can be seen as a discrete pmf - placing a measure on each of the discrete atoms.",
                                                        "The second plot takes the cumulative sum of the weights, forming a CDF. The previous panel plotted 10 of these CDFs",
                                                        "<br> <br>",
                                                        "Notice that the distribution drawn from the DP, as seen in the plot, is discrete over the same space as \\( G_0 \\). For example, if you switch to the Beta distribution, you'll see that the stick-breaking plots produce atoms between 0 and 1.",
                                                        "For low levels of \\( \\alpha \\), just a few atoms get most of the probability mass.", 
                                                        "For high values of \\( \\alpha \\), the probability mass is spread out over more atoms."))
                                          )
                                        )
                              )
                            ),
                   
                   tabPanel("2. DP Posterior",
                             sidebarLayout(
                               sidebarPanel(
                                 sliderInput(inputId = 'pr_lambda',
                                             label = "\\(\\lambda\\) of prior base measure \\(G_0 = Pois(\\lambda)\\):",
                                             min = 1,
                                             max = 25,
                                             value = 20),
                                 sliderInput(inputId = 'alpha_2',
                                             label = "Concentration parameter, \\(\\alpha\\):",
                                             min = 1,
                                             max = 500,
                                             value = 100)
                               ),
                               mainPanel(withMathJax(),
                                         tabsetPanel(
                                           
                                           
                                           tabPanel("2.a. Posterior of a DP",
                                                    tableOutput("freq_tab"),
                                                    plotOutput("PoissonPosterior")
                                           ),
                                           
                                           
                                           tabPanel("2.b. Bayesian Bootstrap"
                                           )
                                         )
                             )
                            )
                   ),
                   
                   tabPanel("3. DP Mixtures"),
                   
                   tabPanel("4. References")
                  
  )
)

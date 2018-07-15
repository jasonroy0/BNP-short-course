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
                                                        "To do this, we need to develop a constructive definition of the DP that tells us how to obtain samples from the process. One such constructive definition is the so-called stick-breaking process.", 
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
                                             min = .001,
                                             max = 500,
                                             value = 100),
                                 sliderInput(inputId = 'n_boot',
                                             label = "Number of Bootstrap/Posterior samples (Panel 2.b.), \\(B\\):",
                                             min = 100,
                                             max = 10000,
                                             value = 100)
                                 
                               ),
                               mainPanel(withMathJax(),
                                         tabsetPanel(
                                           
                                           
                                           tabPanel("2.a. Posterior of a DP",
                                                    HTML("<h1> Posterior of a DP </h1>",
                                                         "Now that we know what draws from the DP look like and how they can be obtained, we will look at one particular use of a DP prior.",
                                                         "Consider observing \\( n=30 \\) count data following some unknown distribution \\(y_1, \\dots, y_{n} | G \\sim G  \\).",
                                                         "Our objective is to make inference about the distribution \\( G\\). To do so in a Bayesian way, we must place a prior over the distribution \\( G \\).",
                                                         "A good candidate for a prior is the DP - a distribution over distributions.",
                                                         "It can be shown that the DP is a conjugate prior. I.e., if \\( G \\sim DP(\\alpha, G_0)  \\), then ",
                                                         "$$G | y_1, \\dots y_{10} \\sim DP(\\alpha + n, \\frac{\\alpha}{\\alpha + n}G_0 + \\frac{n}{\\alpha + n} \\hat{f} )$$",
                                                         "where \\( \\hat{f}(\\cdot) = (1/n) \\sum_{i=1}^{n} \\delta_{y_i}(\\cdot) \\) counts the proportion of observed values equal to the input.",
                                                         "Notice that the posterior mean is just a weighted average of the prior base measure and the observed relative frequency.",
                                                         "<br> <br>",
                                                         "In the plot below, the vertical bars represent the relative frequency of unique observed \\(y_i\\).",
                                                         "The blue line represents the prior base distribution, which is \\(Pois(\\lambda)\\).",
                                                         "The red line is the posterior mean of \\( G \\) - a compromise between the prior and the empirical distribution",
                                                         "The gray lines are draws from the posterior distribution."),
                                                    plotOutput("PoissonPosterior"),
                                                    HTML("If the concentration parameter, \\( \\alpha \\), equals the sample size, then we give equal weight to the prior and observed distributions.", 
                                                         "If \\( \\alpha \\) exceeds the sample size, more weight is given to the prior.",
                                                         "You can verify that as \\( \\alpha \\) increases, the posterior mean is more heavily influenced by the prior",
                                                         "This Bayesian approach is useful because it smooths over the observed distribution.", 
                                                         "Consider that we have no observed data at 18. It is unlikely that there is 0% chance of observing a count of 18.", 
                                                         "The DP posterior uses information from our prior to impute some probability mass at 18 despite not having observed any such values.")
                                           ),
                                           
                                           
                                           tabPanel("2.b. Bayesian Bootstrap",
                                                    HTML('Suppose we want to estimate the mean of the population from which our sample of \\(n=30\\) was drawn.',
                                                         'One possible estimator is the sample mean. One frequentist way to compute the variance of this estimate is through bootstrapping.',
                                                         'Suppose we observe data of sample size \\(n\\) having \\(U\\) unique values, \\( (y_1, y_2, \\dots, y_U) = \\{ y_j\\}_{j \\in 1:U} \\), of data points - each with some frequency.", 
                                                         "Below is an example of some simulated data. We observe the value 18 once, the value 19 five times, etc.'),
                                                    tableOutput("tab"),
                                                    HTML("Dividing the counts by the sample size gives us a table that maps each unique \\(y_j\\) to it's observed frequency distribution, \\( \\hat{f}(y_j) = (1/n) \\sum_{i=1}^{n} I(y_j = y_i) \\).",
                                                         "The frequentist bootstrap takes a random sample (with replacement) of the original data. Then computes the average of this bootstrapped sample. Mathematically,",
                                                         "$$\\bar{x}^{(b)} = \\frac{1}{n}\\sum_{i=1}^n y_i^{(b)} = \\frac{1}{n} \\sum_{j=1}^U \\Big[ y_j\\cdot \\sum_{i=1}^n I( y_j = y_i^{(b)}) \\Big] = \\sum_{j=1}^U \\Big[ y_j\\cdot \\hat{f}^{(b)}(y_j)  \\Big]$$",
                                                         "Above, \\( \\hat{f}^{(b)}(y_j) \\) is the frequency distribtion of the \\( b^{th}\\) bootstrap sample - telling us the relative frequency with which \\(y_j\\) appears in that bootstrap sample.. The point of this re-arrangement is to show that the bootstrap models the unique data points \\(y_j\\) as fixed.",
                                                         "The weights associated with each unique value are the random. When we average across bootstrap samples, we average over the distribution of weights.",
                                                         "The frequentist 'resampling with replacement' formally models the weights as,",
                                                         "$$n\\cdot [\\hat{f}^{(b)}(y_1),\\dots, \\hat{f}^{(b)}(y_j),\\dots, \\hat{f}^{(b)}(y_U)] \\sim Multinomial(N, [\\hat{f}(y_1),\\dots, \\hat{f}(y_j),\\dots, \\hat{f}(y_U)])$$",
                                                         "The frequentist bootstrap draws \\(B\\) weight vectors from this distribution and, with each draw, computes \\( \\bar{x}^{(b)}, \ b\\in\\{1,2,\\dots, B\\} \\).",
                                                         "This set is the sampling distribution, which we use to compute the variance.",
                                                         "The Bayesian bootstrap approach differs slightly. Instead, it treats the multinomial weights,\\([\\hat{f}(y_1),\\dots, \\hat{f}(y_j),\\dots, \\hat{f}(y_U)]\\), as an unknown distribution and places a prior on it. You guessed it, the DP prior - a distribution over distributions - is exactly the prior that is used.",
                                                         "Specifically we place a DP prior with an infinitesmally small value of \\(\\alpha\\). The resulting posterior of the distribution/weights is again a DP (by the conjugacy established in panel 2.a.)",
                                                         "$$[f(y_1),\\dots, f(y_j),\\dots, f(y_U)] \\ \\ | \\ \\ \\vec{y} \\sim DP(N\\cdot [\\hat{f}(y_1),\\dots, \\hat{f}(y_j),\\dots, \\hat{f}(y_U)] )$$",
                                                         "Above, \\( \\vec{y}\\) is shorthand for the observed data. The Bayesian approach draws \\(B\\) weight vectors from this posterior, using each draw to compute \\( \\bar{x}^{(b)} \\sum_{j=1}^U \\Big[ y_j\\cdot f^{(b)}(y_j)  \\Big] \\).", 
                                                         "This set of \\( \\bar{x}^{(b)} \\) is the posterior distribution of the sample average, with which we can compute the variance.",
                                                         "The sampling distribution from the frequentist point of view is then analogous to this posterior distribution."),
                                                    fluidRow(
                                                      column(10, align="center",
                                                             plotOutput("bootPlot", width = "70%")
                                                      )
                                                    ),
                                                    HTML("Notice that as the sample size increases, the two approaches are virtually identical.", 
                                                         "This makes sense since both the multinomial weight distribution and the posterior distribution have the same mean and just about the same variance")
                                           )
                                         )
                             )
                            )
                   ),
                   
                   tabPanel("3. DP Mixtures",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput(inputId = 'gibbs_iter',
                                            label = "Number of MCMC Iterations:",
                                            min = 2,
                                            max = 50,
                                            value = 2, 
                                            animate = T),
                                sliderInput(inputId = 'alpha_3',
                                            label = "Concentration parameter, \\(\\alpha\\):",
                                            min = .001,
                                            max = 500,
                                            value = 100),
                                sliderInput(inputId = 'lambda',
                                            label = "Prior Mean:",
                                            min = 0,
                                            max = 100,
                                            value = 50),
                                sliderInput(inputId = 'tau',
                                            label = "Prior Variance:",
                                            min = 10,
                                            max = 500,
                                            value = 50)
                                
                              ),
                              mainPanel(withMathJax(),
                                        tabsetPanel(
                                          
                                          tabPanel("3.a. Posterior Density Estimation", 
                                                   fluidRow(
                                                     column(10, align="center",
                                                            plotOutput("mixDensity", width = "100%")
                                                     )))
                                          
                                          # tabPanel("3.b. DP-induced Clustering", 
                                          #          fluidRow(
                                          #            column(10, align="center",
                                          #                   plotOutput("mixClass", width = "100%")
                                          #            )))
                                        )
                            )
                            )),
                   
                   tabPanel("4. References")
                  
  )
)

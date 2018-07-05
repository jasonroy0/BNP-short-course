# Repository for BNP Methods for Causal Inference Short Course at JSM 2018
Repository Authors: Jason Roy (@jasonroy0), Mike Daniels (@mjdaniels), Arman Oganisian (@stablemarkets)

Please contact Arman for coding maintenance/GitHub related questions.

## About
This is the official repository for the *Introduction to Bayesian Nonparametric Methods for Causal Inference* short course offered at the Joint Statistical Meeting (JSM) 2018 in Vancouver. More information on the course is provided at the link below.

https://ww2.amstat.org/meetings/jsm/2018/onlineprogram/AbstractDetails.cfm?abstractid=333094

This repository contains much of the code behind the figures and demos covered in the short course.

## Contents
This repository contains R code covering the following topics:
1. Dirichlet Process (DP) Mixtures
    - We have an interactive tutorial for DP methods run through shiny. To run the app within RStudio, run the following lines of code while making sure to have "shiny" app installed

```
install.packages("shiny")
library(shiny)
runGitHub('jasonroy0/BNP-short-course/', username = 'jasonroy0', subdir = 'DP ShinyApp/DPMixApp/')
``` 
2. Gaussian Processes (GP)

## Issues
If you have any issues with the code, please open an issue on GitHub.

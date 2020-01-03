# solarExtremes

A shinyApp package for Estimating Space Weather Benchmarks 

# Installation

Open an RStudio or R session and type: 

devtools::install_github("predsci/solarExtremes")

# Running the ShinyApp

There's only one exported function in the package. In an RStudio session type:

solarExtremes::launchApp()

... or .... 

library(solarExtremes)

launchApp()

# Short Description

The shinyApp solarExtremes provides statistical estimates and uncertainties for the occurence rate of an extreme geomagnetic storm.  The distribution of events are modeled as power law, lognormal and exponential.  The Kolmogorov-Smirnov statistic is used to estimate the goodness of fit. The uncertainty in the estimates is calculated using the bootstrap method and likelihood ratio tests assess whether one distribution is preferred over another. The package includes a data base of geomagnetic (Dst, AE, AL, AP, AU and Ey), interplanetary (Bn, Bz, CMEs, solar wind speed, and plasma beta), and solar (sunspot number and F10.7) indices. The package can also analyze user uploaded data. 

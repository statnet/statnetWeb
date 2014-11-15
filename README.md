statnetWeb
==========

This is an R-shiny interface for the statnet suite of R packages. It currently incorporates functionality from the ergm, network and sna packages, allowing users to fit and assess exponential random graph models via GUI.

Run the app in two ways:  
* Through the shinyapps server with this link: https://ebey.shinyapps.io/statnetWeb  
* On your own machine through RStudio:  
    `install.packages(c("statnet", "shiny", "shinyBS", "RColorBrewer", "lattice", "latticeExtra"))`  
    `shiny::runGitHub("statnetWeb", "statnet")`

*Note:* We will keep the master branch on this page consistent with the version deployed on shinyapps. Active development will occur on separate production branches. No branch other than master should be considered stable.

statnet wiki:  
https://statnet.csde.washington.edu/trac 

statnet packages on CRAN:  
http://cran.r-project.org/web/packages/ergm/  
http://cran.r-project.org/web/packages/network/  
http://cran.r-project.org/web/packages/sna/

#### New in v0.2.0

* Fixed delay when adding terms to formula.
* ergm formula and model fit are cleared when the uploaded network changes.
* Previous goodness of fit plots are cleared when a new model is fit.
* Simulation summary is reformatted so that the simulation parameters and network statistics are listed by row, rather than by column.
* Widgets are disabled when their input is irrelevant for the current network.
* Improved warning boxes for color-coding with more than nine levels.

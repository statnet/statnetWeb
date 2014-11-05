statnetWeb
==========

This is an R-shiny interface for the statnet suite of R packages. It currently incorporates functionality from the ergm, network and sna packages, allowing users to fit and assess exponential random graph models via GUI.

Run the app in two ways:  
* Through the shinyapps server with this link: https://ebey.shinyapps.io/statnetWeb  
* On your own machine through RStudio:  
    `install.packages(c("statnet", "shiny", "shinyBS", "RColorBrewer", "lattice", "latticeExtra"))`
    `shiny::runGitHub("statnetWeb", "statnet")`


statnet packages on CRAN:  
http://cran.r-project.org/web/packages/ergm/  
http://cran.r-project.org/web/packages/network/  
http://cran.r-project.org/web/packages/sna/

statnetWeb
==========

This is an R-shiny interface for the statnet suite of R packages. It currently incorporates functionality from the ergm, network and sna packages, allowing users to fit and assess exponential random graph models via GUI.

Run the app in two ways:  
* Through the shinyapps server with this link: https://statnet.shinyapps.io/statnetWeb  
* On your own machine using the statnetWeb R package:
```r
install.packages("statnetWeb")
statnetWeb::run_sw()
```

statnetWeb wiki:  
https://statnet.csde.washington.edu/trac/wiki/statnetWeb

statnet wiki:  
https://statnet.csde.washington.edu/trac 

statnet packages on CRAN:  
http://cran.r-project.org/web/packages/ergm/  
http://cran.r-project.org/web/packages/network/  
http://cran.r-project.org/web/packages/sna/

#### New in v0.3.6

* New sortable table of attributes
* New citations in addition to BibTeX
* Refresh positions of nodes in network plot
* run_sw() now uses system's default web browser to launch application
* Can color code nodes by mode for bipartite network
* Change shape of node depending on mode for bipartite network

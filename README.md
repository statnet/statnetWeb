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

#### New in v0.3.4

* Conditional uniform graph test histograms in Network Descriptives
* Descriptions of built-in networks
* Minor bug fixes

#### New in v0.3.3

* Fix bug from v0.3.2 preventing built-in networks from loading
* Prevent calls to search.ergmTerms() from printing to the console

#### New in v0.3.2

* App is compatible with Shiny v0.11.1.9002 (we have a new look!)
* Only .rds files supported when saving and uploading from the R environment.
* Issue solved where GOF plots from directed networks were not appearing.
* Issue with uploading edge values solved.

#### New in v0.3.1

* Improved method of saving user-entered attributes
* Bug in uploading relational data saved as R object is fixed
* "Fit Model" button is closer to summary statistics
* User can add terms to formula by pressing Enter key
* User is not allowed to color code degree distribution of total degree for directed networks (in- and out-degree distributions are allowed)
* Title of application is incorporated into title of first tab
* Prevented error in viewing GOF comparison when no models are saved
* New color palettes for attributes with more than nine levels

#### New in v0.3.0

* Access term documentation within Fit Model page
* Customize MCMC control parameters when fitting a model or simulating from a model
* Save up to five different models for a single network
* Compare coefficients and AIC/BIC scores across models
* Switch between models when looking at MCMC Diagnostics, GOF and Simulations
* Compare GOF plots across saved models in a comprehensive chart
* Plot simulation statistics compared to target statistics
* Arrow icons for page navigation

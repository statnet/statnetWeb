#' ergm-common, server.R
#' ======================

#+ eval=FALSE
#make sure that both shiny and statnet packages are loaded
library(shiny)
library(statnet)

shinyServer(
  function(input, output){
    
    #load datasets
    data(ecoli)
    data(florentine)
    data(fauxhigh)
    data(faux.mesa.high)
    data(kapferer)
    data(sampson)
    
#' Saving the following vectors of terms will allow us to only display the terms
#' that are applicable to a certain network. These don't depend on any user input
#' and will never change value, so they don't need to be in a reactive expression.
#+ eval=FALSE    
    dir.terms <- c('edges', 'nodefactor', 'nodematch', 'nodemix', 'nodecov',
                   'absdiff', 'gwesp', 'mutual', 'idegree', 'odegree')
    undir.terms <- c('edges', 'nodefactor', 'nodematch', 'nodemix', 'nodecov',
                     'absdiff', 'gwesp', 'degree', 'b1degree', 'b2degree', 
                     'mindegree', 'triangles')
    

#' Reactive Expressions
#' ---------------------------------
#' These expressions contain most of the code from the ergm package
#' that we will be using. Objects created with a reactive expression
#' can be accessed from any other reactive expression or render functions
#' and they only get re-run when their values are outdated. Since many of 
#' our render functions will be calling the same ergm objects, using 
#' reactive expressions will help the app run much faster.
#'
#' Notice that already in this chunk of code we call previously declared reactive
#' objects. For example, to use the reactive list of vertex attributes in the
#' definition of the numeric vertex attributes, we call `attr()`.    
#+ eval=FALSE
    nw.reac <- reactive({eval(parse(text = input$dataset))})
    #number of nodes in nw
    nodes <- reactive({nw.reac()$gal$n}) 
    #get coordinates to plot network with
    coords <- reactive({plot.network(eval(parse(text = input$dataset)))})
    
    #list of vertex attributes in nw
    attr <- reactive({
      attr <- c()
      if(input$dataset != ''){      
        attr<-list.vertex.attributes(nw.reac())
      }
      attr}) 
    
    #numeric attributes only (for size menu)
    numattr <- reactive({
      numattr <- c()
      if(input$dataset != ''){  
        for(i in 1:length(attr())){
          if(is.numeric(get.vertex.attribute(nw.reac(),attr()[i]))){
            numattr <- append(numattr,attr()[i])
          } 
        }} 
      numattr})
    
#' Some ergm terms (e.g. `gwesp`, `degree` and `nodematch`) take in their own arguments. 
#' The following reactive expressions take user input and create vectors that can later
#' be used as terms in an ergm formula.
#+ eval=FALSE    
    gwesp.terms <- reactive({
      gterms <- paste("gwesp(",input$choosegwesp,
                      ", fixed = ",input$fixgwesp,")", sep="")
      if (!any(input$terms == 'gwesp')){
        gterms <- NULL
      }
      gterms})
    
    degree.terms <- reactive({
      dterms <- paste("degree(",input$choosedegree,")", sep="")
      if(!any(input$terms == 'degree')){
        dterms <- NULL
      }
      dterms})
    
    nodematch.terms <- reactive({
      nterms <- paste("nodematch('",input$choosenodematch,"')", sep="")
      if(!any(input$terms == 'nodematch')){
        nterms <- NULL
      }
      nterms})

#' `ergm.terms` is a compilation of all the terms entered, without redundancies,
#' which we then use to create a complete formula. 
#' 
#+ eval=FALSE    
    ergm.terms <- reactive({
      interms <- input$terms
      if (any(interms == 'gwesp')) {
        interms <- interms[-which(interms == 'gwesp')]}
      if (any(interms == 'degree')) {
        interms <- interms[-which(interms == 'degree')]}
      if (any(interms == 'nodematch')) {
        interms <- interms[-which(interms == 'nodematch')]}
      paste(c(interms, gwesp.terms(), degree.terms(), 
              nodematch.terms()), sep = '', collapse = '+')})
    
    ergm.formula <- reactive({
      formula(paste('nw.reac() ~ ',ergm.terms(), sep = ''))})

#' Once we have a formula, creating a model object, checking the goodness of fit
#' and simulating from it is similar to what we would write in the command line,
#' wrapped in a reactive statement.

#+ eval=FALSE 
    model1.reac <- reactive({ergm(ergm.formula())})
    
    #use default gof formula
    model1.gof1 <- reactive({gof(model1.reac())})
    
    #use gof term that user specifies
    gof.form <- reactive({
      formula(paste('model1.reac() ~ ', input$gofterm, sep = ''))})
    model1.gof2 <- reactive({gof(gof.form())})

    model1.sim.reac <- reactive({
      simulate(model1.reac(), nsim = input$nsims)})
    
    #get coordinates to plot simulations with
    sim.coords.1 <- reactive({plot.network(model1.sim.reac())})
    sim.coords.2 <- reactive({plot.network(model1.sim.reac()[[input$this.sim]])})
    

#' Output Expressions
#' ---------------------------
#' Every piece of content that gets displayed in the app has to be
#' rendered by the appropriate `render*` function, e.g. `renderPrint` for text 
#' and `renderPlot` for plots. Most of the render functions here call 
#' reactive objects that were created above. I have divided the output objects
#' into sections depending on what tab of the app they are called from.
#'    
#' **Plot Network**  
#' 
#' Because the menu options for coloring/sizing the nodes on a network plot 
#' depend on which network has been selected, we have to dynamically render 
#' these input menus, rather than statically defining them in `ui.R`.  
#' including 
#' 
#+ eval=FALSE
    output$dynamiccolor <- renderUI({
      selectInput('colorby',
                  label = 'Color nodes according to:',
                  c('None' = 2, attr()),
                  selectize = FALSE)
    })
    
    output$dynamicsize <- renderUI({
      selectInput('sizeby',
                  label = 'Size nodes according to:',
                  c('None' = 1, numattr()),
                  selectize = FALSE)
    })


    output$nwplot <- renderPlot({
      if (input$goButton == 0){
        return()
      }
      input$goButton
      nw <- isolate({nw.reac()})
      #scale size of nodes onto range between .7 and 3.5
      minsize <- min(get.vertex.attribute(nw,input$sizeby))
      maxsize <- max(get.vertex.attribute(nw,input$sizeby))
      if (input$sizeby == '1'){
        size = 1
      } else { 
        size = (get.vertex.attribute(nw,input$sizeby)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
      }
      
      plot.network(nw, coord = coords(), 
                   displayisolates = input$iso, 
                   displaylabels = input$vnames, 
                   vertex.col = input$colorby,
                   vertex.cex = size)
    })


    output$attr <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      nw <- isolate(nw.reac())
      return(nw)
    })
########################################
    output$check1 <- renderPrint({
      ergm.terms()
    })
    output$currentdataset <- renderPrint({
      input$dataset
    })
    output$check2 <- renderPrint({
      ergm.terms()
    })
    output$currentdataset2 <- renderPrint({
      input$dataset
    })
    output$check3 <- renderPrint({
      ergm.terms()
    })
    output$currentdataset3 <- renderPrint({
      input$dataset
    })
    output$check4 <- renderPrint({
      ergm.terms()
    })
    output$currentdataset4 <- renderPrint({
      input$dataset
    })
    
    
    
    output$dynamicdegree <- renderUI({
      selectInput('choosedegree', 
                  label = 'Choose degree(s)',
                  paste(0:(as.numeric(nodes())-1)),
                  multiple = TRUE,
                  selectize = FALSE)
    })
    
    output$dynamicnodematch <- renderUI({
      selectInput('choosenodematch', 
                  label = 'Attribute for nodematch',
                  attr(),
                  multiple = TRUE,
                  selectize = FALSE)
    })
    
    
    
    



    output$listofterms <- renderUI({
      if(nw.reac()$gal$directed){
        current.terms <- c(dir.terms)
      } else {
        current.terms <- c(undir.terms)
      }
      selectInput('terms',label = 'Choose term(s):',
                  current.terms,
                  selected='edges',
                  multiple=TRUE, 
                  selectize = FALSE)
    })
    
    output$modelfit <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$fitButton == 0){
        return('Please choose term(s) for the model')
      }
      model1 <- isolate(model1.reac())
      return(summary(model1))
    })
    
    
    output$sim.summary <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$simButton == 0){
        return("You haven't clicked 'Simulate' yet!")
      }
      model1.sim <- isolate(model1.sim.reac())
      return(summary(model1.sim))
    })
    
    
    output$simplot <- renderPlot({
      input$goButton
      if(input$simButton == 0){
        return()
      }
      nw <- isolate(nw.reac())
      nsims <- isolate(input$nsims)
      model1.sim <- isolate(model1.sim.reac()) 
      
      #can't plot simulation number greater than total sims
      if(input$this.sim > nsims){
        return()
      } 
      #scale size of nodes onto range between .7 and 3.5
      minsize <- min(get.vertex.attribute(nw,input$sizeby))
      maxsize <- max(get.vertex.attribute(nw,input$sizeby))
      if (input$sizeby == '1'){
        size = 1
      } else { 
        size = (get.vertex.attribute(nw,input$sizeby)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
      }
      #need new plot display options on sidebar
      #add sizing option eventually    
          
      if (nsims == 1){
        
        plot(model1.sim, coord = sim.coords.1(), 
             displayisolates = input$iso, 
             displaylabels = input$vnames, 
             vertex.col = input$colorby,
             vertex.cex = size)
      } else {
        plot(model1.sim[[input$this.sim]], 
             coord = sim.coords.2(),
             displayisolates = input$iso, 
             displaylabels = input$vnames, 
             vertex.col = input$colorby,
             vertex.cex = size)
      }
    })
    
    
    output$gof.summary <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$fitButton == 0){
        return('Please choose term(s) for the model on the "Fit Model" tab')
      } 
      else if (input$gofButton == 0){
        return(p('Choose a term for checking the goodness-of-fit, or just click
                 "Run" to use the default formula'))
      }
      
      isolate(if (input$gofterm == ''){
        model1.gof <- model1.gof1()
      } else {
        model1.gof <-model1.gof2()})
      
      return(model1.gof)
      })
    
    
    output$gofplot <- renderPlot({   
      if (input$goButton == 0){
        return()
      } else if (input$fitButton == 0){
        return()
      } else if (input$gofButton == 0){
        return()
      }
      
      isolate(if (input$gofterm == ''){
        model1.gof <- model1.gof1()
        par(mfrow=c(1,3))
      } else {
        par(mfrow=c(1,1))
        model1.gof <- model1.gof2()})
      
      plot.gofobject(model1.gof)
      par(mfrow=c(1,1))
    })
    
    output$diagnostics <- renderPrint({
      model1 <- model1.reac()
      if (is.null(model1$sample)){
        return('MCMC did not run to fit this model')
      } else {
        mcmc.diagnostics(model1)
      }
    })
    
  })
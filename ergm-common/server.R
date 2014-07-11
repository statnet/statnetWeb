#ergm-undir
#server.R
library(shiny)
library(statnet)

shinyServer(
  function(input, output){
    
    
    data(ecoli)
    data(florentine)
    data(fauxhigh)
    data(faux.mesa.high)
    data(kapferer)
    
    ##########################
    ## reactive expressions ##
    
    nw.reac <- reactive({eval(parse(text = input$dataset))})
    nodes <- reactive({nw.reac()$gal$n}) #number of nodes in nw
    
    #list of attributes in nw
    attr <- reactive({
      attr <- ''
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
      nterms <- paste("nodematch(",input$choosenodematch,")", sep="")
      if(!any(input$terms == 'nodematch')){
        nterms <- NULL
      }
      nterms})
    
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
    
    model1.reac <- reactive({ergm(ergm.formula())})
    
    model1.sim.reac <- reactive({
      simulate(model1.reac(), nsim = input$nsims)})
    
    model1.gof1 <- reactive({gof(model1.reac())})
    
    gof.form <- reactive({
      formula(paste('model1.reac() ~ ', input$gofterm, sep = ''))})
    
    model1.gof2 <- reactive({gof(gof.form())})
    
    
    
    ########################
    ## output expressions ##
    
    output$check1 <- renderPrint({
      ergm.terms()
    })
    
    output$currentdataset <- renderPrint({
      input$goButton
      isolate(input$dataset)
    })
    
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
    
    output$nwplot <- renderPlot({
      if (input$goButton == 0){
        return()
      } 
      nw <- isolate(nw.reac())
      # eventually add vertex.cex = input$sizeby
      plot.network(nw, displayisolates = input$iso, 
                   displaylabels = input$vnames, 
                   vertex.col = input$colorby,
                   vertex.cex = input$sizeby)
    })
    
    
    output$attr <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      nw <- isolate(nw.reac())
      return(nw)
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
      
      #can't plot simulation number greater than total sims
      if(input$this.sim > nsims){
        return()
      }      
      #use plot display options from sidebar
      #add sizing option eventually    
      model1.sim <- isolate(model1.sim.reac())     
      if (nsims == 1){
        plot(model1.sim, displayisolates = input$iso, 
             displaylabels = input$vnames, 
             vertex.col = input$colorby)
      } else {
        plot(model1.sim[[input$this.sim]], 
             displayisolates = input$iso, 
             displaylabels = input$vnames, 
             vertex.col = input$colorby)
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
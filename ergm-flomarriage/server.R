# ergm-flomarriage
# server.R
library(shiny)
library(RColorBrewer)
library(statnet)


shinyServer(
  function(input, output) {
    
    data(florentine)   
    
    #nw <- reactive({eval(parse(text = input$dataset))})
    
    
    output$nwplot <- renderPlot({
      if (input$goButton == 0){
        return()
      }
      nw <- isolate(eval(parse(text = input$dataset)))
      
      #could isolate iso, vnames, vcolor, vsize if you don't want them to update automatically
      vcolor <- switch(input$colorby,
                       'None' = 2,
                       'priorates' = 'priorates',
                       'totalties' = 'totalties',
                       'vertex.names' = 'vertex.names',
                       'wealth' = 'wealth')
      vsize <- switch(input$sizeby,               
                      'priorates' = get.vertex.attribute(nw,'priorates')/25,
                      'totalties' = get.vertex.attribute(nw,'totalties')/25,
                      'vertex.names' = ,
                      'None' = 0.9,
                      'wealth' = get.vertex.attribute(nw,'wealth')/25)
      
      plot(nw, displayisolates = input$iso, displaylabels = input$vnames, vertex.col = vcolor, vertex.cex = vsize)
    })
    
    output$attr <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      nw <- isolate(eval(parse(text = input$dataset)))
      return(nw)
    })
    
    output$modelfit <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$fitButton == 0){
        return('Please choose term(s) for the model')
      }
      nw <- isolate(eval(parse(text = input$dataset)))
      ergm.terms <- isolate(paste(input$terms, sep = '', collapse = '+'))
      ergm.formula <- isolate(formula(paste('nw ~ ',ergm.terms, sep = '')))
      
      model1 <- ergm(ergm.formula) #this changes with terms!
      return(summary(model1))
    })
    
    output$sim.summary <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$simButton == 0){
        return("You haven't clicked 'Simulate' yet!")
      }
      nw <- isolate(eval(parse(text = input$dataset)))
      ergm.terms <- isolate(paste(input$terms, sep = '', collapse = '+'))
      ergm.formula <- formula(paste('nw ~ ',ergm.terms, sep = ''))
      
      model1 <- ergm(ergm.formula) #this changes with terms!
      model1.sim <- isolate(simulate(model1, nsim = input$nsims))
      return(summary(model1.sim))
    })
    
    output$simplot <- renderPlot({
      input$goButton
      if(input$simButton == 0){
        return()
      }
      nsims <- isolate(input$nsims)
      
      #can't plot simulation number greater than total sims
      if(input$this.sim > nsims){
        return()
      }
      
      nw <- isolate(eval(parse(text = input$dataset)))
      ergm.terms <- isolate(paste(input$terms, sep = '', collapse = '+'))
      ergm.formula <- formula(paste('nw ~ ',ergm.terms, sep = ''))
      
      model1 <- ergm(ergm.formula) #this changes with terms!
      model1.sim <- isolate(simulate(model1, nsim = nsims))
      
      #use plot display options from sidebar
      vcolor <- switch(input$colorby,
                       'None' = 2,
                       'priorates' = 'priorates',
                       'totalties' = 'totalties',
                       'vertex.names' = 'vertex.names',
                       'wealth' = 'wealth')
      vsize <- switch(input$sizeby,               
                      'priorates' = get.vertex.attribute(nw,'priorates')/25,
                      'totalties' = get.vertex.attribute(nw,'totalties')/25,
                      'vertex.names' = ,
                      'None' = 0.9,
                      'wealth' = get.vertex.attribute(nw,'wealth')/25)
      
      
      if (nsims == 1){
        plot(model1.sim, displayisolates = input$iso, displaylabels = input$vnames, vertex.col = vcolor, vertex.cex = vsize)
      } else {
        plot(model1.sim[[input$this.sim]], displayisolates = input$iso, displaylabels = input$vnames, vertex.col = vcolor, vertex.cex = vsize)
      }
    })
    
    output$gof.summary <- renderPrint({
      if (input$goButton == 0){
        return('Please choose a sample dataset from the side panel')
      }
      else if (input$fitButton == 0){
        return('Please choose term(s) for the model on the "Fit Model" tab')
      }
      
      nw <- isolate(eval(parse(text = input$dataset)))
      ergm.terms <- isolate(paste(input$terms, sep = '', collapse = '+'))
      ergm.formula <- formula(paste('nw ~ ',ergm.terms, sep = ''))
      
      model1 <- ergm(ergm.formula) #this changes with terms!
      
      if (input$gofterm == ''){
        model1.gof <- gof(model1)
      } else {
        gof.formula <- formula(paste('model1 ~ ', input$gofterm, sep = ''))
        model1.gof <- gof(gof.formula)}
      return(model1.gof)
    })
    
    output$gofplot <- renderPlot({
      nw <- isolate(eval(parse(text = input$dataset)))
      ergm.terms <- isolate(paste(input$terms, sep = '', collapse = '+'))
      ergm.formula <- formula(paste('nw ~ ',ergm.terms, sep = ''))
      
      model1 <- ergm(ergm.formula) #this changes with terms!
      if (input$gofterm == ''){
        model1.gof <- gof(model1)
        par(c(3,1))
      } else {
        par(c(1,1))
        gof.formula <- formula(paste('model1 ~ ', input$gofterm, sep = ''))
        model1.gof <- gof(gof.formula)}
      plot.gofobject(model1.gof)
    })
    
    
    
  })
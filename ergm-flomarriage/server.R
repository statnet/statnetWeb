# ergm-flomarriage
# server.R
library(shiny)
library(RColorBrewer)
library(statnet)


shinyServer(
  function(input, output) {
    
    data(florentine)   
    
    ##########################
    ## reactive expressions ##
    
    nw.reac <- reactive({eval(parse(text = input$dataset))})
    ergm.terms <- reactive({paste(input$terms, sep = '', collapse = '+')})
    ergm.formula <- reactive({formula(paste('nw.reac() ~ ',ergm.terms(), sep = ''))})
    model1.reac <- reactive({ergm(ergm.formula())})
    model1.sim.reac <- reactive({simulate(model1.reac(), nsim = input$nsims)})
    model1.gof1 <- reactive({gof(model1.reac())})
    gof.form <- reactive({formula(paste('model1.reac() ~ ', input$gofterm, sep = ''))})
    model1.gof2 <- reactive({gof(gof.form())})
    
    
    ########################
    ## output expressions ##    
    
    output$nwplot <- renderPlot({
      if (input$goButton == 0){
        return()
      } 
      
      nw <- isolate(nw.reac())
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
                      'None' = 1,
                      'wealth' = get.vertex.attribute(nw,'wealth')/25)
      
      plot.network(nw, displayisolates = input$iso, displaylabels = input$vnames, vertex.col = vcolor, vertex.cex = vsize)
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
      model1.sim <- isolate(model1.sim.reac())     
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
    
  })
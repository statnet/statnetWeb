#' ---
#' title: "ergm-common, server.R"
#' author: "Emily Beylerian"
#' ---
#' ergm-common
#' ============
#' server.R
#' =========

#' **Before reading this document:** The Shiny app "ergm-common" is not contained in a
#' single R Script. Within the folder "ergm-common" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files (just a
#' .png file right now). If you are unfamiliar with Shiny apps, it may be more 
#' natural and helpful to start with the documentation for `ui.R` and then move on to
#' `server.R`.
#' 
#' Every `server.R` script contains an unnamed function inside a call to `shinyServer`.
#' The job of the unnamed function is to take input elements from the user and define
#' output elements that will be displayed in the app. For more information on how this
#' works, see [the Shiny tutorial](http://shiny.rstudio.com/tutorial/lesson4/).
#' 
#' Also notice that in this block of code we loaded the necessary packages (shinyIncubator
#' contains the functionality for the progress bar) outside of `shinyServer` and 
#' inside loaded all of the datasets we might need.
#+ eval=FALSE

library(shiny)
library(shinyIncubator)
library(statnet)

shinyServer(
  function(input, output, session){
    
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
                     'mindegree', 'triangle')
    unip.terms <- c('edges', 'nodefactor', 'nodematch', 'nodemix', 'nodecov', 
                    'absdiff', 'mutual','degree', 'idegree', 'odegree', 'mindegree',
                    'triangle', 'gwesp')
    bip.terms <- c('edges', 'nodefactor', 'nodematch', 'nodemix', 'nodecov', 
                   'absdiff', 'mutual', 'b1degree', 'b2degree')

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
    nw.reac <- reactive({
				input$goButton
				isolate(eval(parse(text = input$dataset)))
			})
    #number of nodes in nw
    nodes <- reactive({
				input$goButton
				isolate(nw.reac()$gal$n)}) 
    #get coordinates to plot network with
    coords <- reactive({
				input$goButton
				isolate(plot.network(eval(parse(text = input$dataset))))})
    
    #list of vertex attributes in nw
    attr <- reactive({
				
    	  input$goButton
      
    	  attr <- c()
          if(input$dataset != ''){      
    		  isolate(  attr<-list.vertex.attributes(nw.reac()))
          }
          attr
      }) 
    
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
    model1.reac <- reactive({
      input$fitButton
      isolate(ergm(ergm.formula()))})
    
    #use default gof formula
    model1.gof <- reactive({
      input$gofButton
      if(input$gofterm == ''){
        model1.gof <- gof(model1.reac())
      } else {
        gof.form <- formula(paste('model1.reac() ~ ', input$gofterm, sep = ''))
        model1.gof <- gof(gof.form)
      }
      isolate(model1.gof)})
    

    model1.sim.reac <- reactive({
      input$simButton
      isolate(simulate(model1.reac(), nsim = input$nsims))})
    
    #get coordinates to plot simulations with
    sim.coords.1 <- reactive({
      input$simButton
      isolate(plot.network(model1.sim.reac()))})
    sim.coords.2 <- reactive({
      input$simButton
      isolate(plot.network(model1.sim.reac()[[input$this.sim]]))})
    

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
#' The network plot takes display options from the sidebar of the ui. Even though 
#' I set the value of the 'None' option in the `sizeby` menu (above) as `1`, it gets
#' coerced into the string `'1'` by the rest of the strings in the vector of menu 
#' options. The variable `size` takes the value 1 if the user wants all the nodes
#' to be the same size, and otherwise maps the values of the numeric attributes into 
#' the range between .7 and 3.5 using the formula $y = (x-a)/(b-a) * (d-c) + c$, where
#' $x$ is the input in some range $[a,b]$ and $y$ is the output in range $[c,d]$.

#+ eval=FALSE
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

    #summary of network attributes
    output$attr <- renderPrint({
      if (input$goButton == 0){
        return(cat('Please choose a sample dataset from the side panel'))
      }
      nw <- isolate(nw.reac())
      return(nw)
    })

#' **Fit Model**
#' 
#' The user is only allowed to change the dataset on the first tab; on the
#' following tabs I output the current dataset as a reminder of what network
#' they are working with. 
#' 
#' Like the coloring and sizing options in the network plot, the `selectInput`
#' menus for creating an ergm formula must be dynamically rendered. Right now 
#' the total list of terms available is from the statnet
#' [list of common terms](http://statnet.csde.washington.edu/EpiModel/nme/2014/d2-ergmterms.html).
#' The terms that the user sees in the menu depends on whether the current 
#' network is directed or undirected (future: bipartite/independent).
#' 
#' The `selectInput` menus for `degree` and `nodematch` (more coming soon) depend
#' on the number of nodes in the network and the vertex attributes, respectively.

#+ fitmodel1, eval=FALSE
    output$currentdataset1 <- renderPrint({
      cat(input$dataset)
    })

    output$listofterms <- renderUI({
      if(nw.reac()$gal$directed & nw.reac()$gal$bipartite){
        current.terms <- intersect(dir.terms, bip.terms)
      } else if(nw.reac()$gal$directed) {
        current.terms <- intersect(dir.terms, unip.terms)
      } else if(nw.reac()$gal$bipartite){
        current.terms <- intersect(undir.terms, bip.terms)
      } else if(!nw.reac()$gal$bipartite & !nw.reac()$gal$bipartite){
        current.terms <- intersect(undir.terms, unip.terms)
      }
      selectInput('terms',label = 'Choose term(s):',
                  current.terms,
                  selected='edges',
                  multiple=TRUE, 
                  selectize = FALSE)
    })

    output$dynamicdegree <- renderUI({
      selectInput('choosedegree', 
                  label = 'Choose degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
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
    output$dynamicnodemix <- renderUI({
      selectInput('choosenodemix',
                  label = 'Attribute for nodemix',
                  attr(),
                  multiple = TRUE,
                  selectize = FALSE)
    })

#' Below I output the current formulation of the ergm 
#' model so the user can clearly see how their menu selections change the model.
#' Since `ergm.terms()` is a reactive object, it will automatically update when
#' the user clicks on menu options.
#'  
#+ fitmodel2, eval=FALSE
    output$checkterms1 <- renderPrint({
      cat(ergm.terms())
    })
    
    output$modelfit <- renderPrint({
      if (input$fitButton == 0){
        return(cat('Please choose term(s) for the model'))
      }
      model1.reac()
    })

    output$modelfitsum <- renderPrint({
      if (input$fitButton == 0){
        return(cat('Please choose term(s) for the model'))
      }
      summary(model1.reac())
    })

#' **Diagnostics - Goodness of Fit**
#' 
#' Again, I output the current dataset and the ergm formula for the user to verify.
#' One drawback of the `navbarPage` layout option (we specified this in the top of
#' `ui.R`) is that you can't specify certain elements or panels to show up on 
#' multiple pages. Furthermore, as far as I can tell, Shiny will not let you use 
#' the same piece of output from `server.R` twice in `ui.R`. Therefore, 
#' `output$currentdataset2` and `output$check2` are the same as `output$currentdataset`
#' and `output$check1` with different names.
#' 
#' In the reactive section above the creation of `model1.gof` depends on the term the 
#' user inputs. After checking that the user has already clicked the `actionButton`
#' on the page we can output the text of the gof object and the plot of the gof object.
#+ eval=FALSE
    output$currentdataset2 <- renderPrint({
      cat(input$dataset)
    })

    output$checkterms2 <- renderPrint({
      cat(ergm.terms())
    })
    
    output$gof.summary <- renderPrint({
      if (input$gofButton == 0){
        return(cat('Choose a term for checking the goodness-of-fit, or just click
                 "Run" to use the default formula'))
      }
      
      return(isolate(model1.gof()))
      })
    
    
    output$gofplot <- renderPlot({   
      if (input$gofButton == 0){
        return()
      }
      gofterm <- isolate(input$gofterm)
      if (gofterm == ''){
        par(mfrow=c(3,1))
      } else {
        par(mfrow=c(1,1))
      }
      
      isolate(plot.gofobject(model1.gof()))
      par(mfrow=c(1,1))
    })

    output$gofplotspace <- renderUI({
      input$gofButton
      gofterm <- isolate(input$gofterm)
      if (gofterm == ''){
        gofplotheight = 1000
      } else {
        gofplotheight = 400
      }
      plotOutput('gofplot', height=gofplotheight)
    })

#' **Diagnostics - MCMC Diagnostics**
#' 
#' When using the `mcmc.diagnostics` function in the command line, the printed 
#' diagnostics and plots all output together. In Shiny, the `renderPrint` function
#' can capture all the printed output without the plot, but `renderPlot` cannot 
#' capture the plot. For now, we only display the printed output (unitl `mcmc.diagnostics`
#' has an option to separate the plot).
#' 
#+ eval=FALSE

    output$checkterms3 <- renderPrint({
      cat(ergm.terms())
    })
    output$currentdataset3 <- renderPrint({
      cat(input$dataset)
    })
    
    output$diagnostics <- renderPrint({
      model1 <- model1.reac()
      if (is.null(model1$sample)){
        return(cat('MCMC did not run to fit this model'))
      } else {
        mcmc.diagnostics(model1)
      }
    })

    output$diagnosticsplot <- renderPlot({
      plot.ergm(model1.reac())
    })

#' **Simulations**
#' 
#' On this page the user can choose how many simulations of the model to run. The 
#' reactive object `model1.sim.reac` contains all the simulations, which we can output
#' a summary of and choose one simulation at a time to plot. *Note:* when the user
#' chooses to simulate one network, `model1.sim.reac()` is a reactive object of class
#' network. When the user chooses to simulate multiple networks, `model1.sim.reac()`
#' contains a list of the generated networks. This is why we have to split up the plot
#' command in an if-statement. The rest of the display options should look familiar
#' from the 'Network Plot' tab.
#+ eval=FALSE
    output$checkterms4 <- renderPrint({
      cat(ergm.terms())
    })
    output$currentdataset4 <- renderPrint({
      cat(input$dataset)
    })
    

    output$sim.summary <- renderPrint({
      if (input$simButton == 0){
        return()
      }
      model1.sim <- isolate(model1.sim.reac())
      if (isolate(input$nsims) == 1){
        return(model1.sim)
      }
      return(summary(model1.sim))
    })

    output$dynamiccolor2 <- renderUI({
      selectInput('colorby2',
                  label = 'Color nodes according to:',
                  c('None' = 2, attr()),
                  selectize = FALSE)
    })
    
    output$dynamicsize2 <- renderUI({
      selectInput('sizeby2',
                  label = 'Size nodes according to:',
                  c('None' = 1, numattr()),
                  selectize = FALSE)
    })
    
    
    output$simplot <- renderPlot({
      if(input$simButton == 0){
        return()
      }
      nw <- nw.reac()
      nsims <- isolate(input$nsims)
      model1.sim <- isolate(model1.sim.reac()) 
      
      #can't plot simulation number greater than total sims
      if(input$this.sim > nsims){
        return()
      } 
      #scale size of nodes onto range between .7 and 3.5
      minsize <- min(get.vertex.attribute(nw,input$sizeby2))
      maxsize <- max(get.vertex.attribute(nw,input$sizeby2))
      if (input$sizeby2 == '1'){
        size = 1
      } else { 
        size = (get.vertex.attribute(nw,input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
      }
      #need new plot display options on sidebar
      #add sizing option eventually    
          
      if (nsims == 1){
        
        plot(model1.sim, coord = sim.coords.1(), 
             displayisolates = input$iso2, 
             displaylabels = input$vnames2, 
             vertex.col = input$colorby2,
             vertex.cex = size)
      } else {
        plot(model1.sim[[input$this.sim]], 
             coord = sim.coords.2(),
             displayisolates = input$iso2, 
             displaylabels = input$vnames2, 
             vertex.col = input$colorby2,
             vertex.cex = size)
      }
    })
    
    
  })

#' ---
#' title: "ergm-app, server.R"
#' author: "Emily Beylerian"
#' ---
#' ergm-app
#' ===========
#' server.R
#' =========

#' **Before reading this document:** The Shiny app "ergm-app" is not contained in a
#' single R Script. Within the folder "ergm-app" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files (just a
#' .png file right now). If you are unfamiliar with Shiny apps, it may be more 
#' natural and helpful to start with the documentation for `ui.R` and then move on to
#' `server.R`.
#' 
#' **Basics**
#' 
#' Every `server.R` script contains an unnamed function inside a call to `shinyServer`.
#' The job of the unnamed function is to take input elements from the user and define
#' output elements that will be displayed in the app. For more information on how this
#' works, see [the Shiny tutorial](http://shiny.rstudio.com/tutorial/lesson4/). 
#' If the function is empty, e.g.
#' ```
#' shinyServer(
#'  function(input,output){})
#' ```
#' the UI elements will still be displayed without any dynamic content.
#' 
#' To create dynamic content, we will mainly rely on three types of expressions:
#' 
#' *Reactive:* Reactive expressions are expressions that can read reactive values and
#' call other reactive expressions. Whenever a reactive value changes, any reactive
#' expressions that depended on it are marked as "invalidated" and will automatically
#' re-execute if necessary. If a reactive expression is marked as invalidated, any other
#' reactive expressions that recently called it are also marked as invalidated. In this
#' way, invalidations ripple through the expressions that depend on each other. Reactive
#' expressions use lazy evaluation; when their dependencies change, they don't re-execute
#' right away but rather wait until they are called by someone else.
#' 
#' *Observer:* An observer is like a reactive expression in that it can read reactive
#' values and call reactive expressions, and will automatically re-execute when those
#' dependencies change. But unlike reactive expressions, it doesn't yield a result and
#' can't be used as an input to other reactive expressions. Thus, observers are only useful
#' for their side effects (for example, performing I/O). Observers use eager evaluation;
#' as soon as their dependencies change, they schedule themselves to re-execute.
#' 
#' *Render:* The `render*` functions are responsible for converting content to the form
#' in which it should be displayed. Before assigning an object to an element of `output`,
#' it gets passed to the appropriate `render*` function.
#' 
#' **Code**
#' 
#+ eval=FALSE

library(shiny)
library(statnet)
library(RColorBrewer)

data(faux.mesa.high)

shinyServer(
  function(input, output, session){
    
    
#' Saving the following vectors of terms will allow us to only display the terms
#' that are applicable to a certain network. These don't depend on any user input
#' and will never change value, so they don't need to be in a reactive expression.
#+ eval=FALSE    
    dir.terms <- c('absdiff', 'idegree', 'odegree', 'edges', 'gwesp', 'mutual', 
                   'nodefactor', 'nodematch', 'nodemix', 'nodecov')
    undir.terms <- c('absdiff', 'b1degree', 'b2degree', 'degree', 'edges', 'gwesp',
                     'nodecov', 'nodefactor', 'nodematch', 'nodemix',
                     'triangle')
    unip.terms <- c('absdiff', 'degree', 'idegree', 'odegree','edges', 'gwesp', 
                    'mutual', 'nodecov', 'nodefactor', 'nodematch',
                    'nodemix', 'triangle')
    bip.terms <- c('absdiff', 'b1degree', 'b2degree', 'edges', 'mutual', 'nodefactor',
                   'nodematch', 'nodemix', 'nodecov')

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
    #initial network - use to set the initial values of network attributes
    nwinit <- reactive({
      nw <- network(read.table(paste(input$rawdata[1,4])))
      if(input$fmh){
        nw <- faux.mesa.high
      }
      nw
    })
    #network to do all work with, make changes, plot, etc
    nwreac <- reactive({
        #datapath is stored in 4th column of dataframe in input$rawdata
        #network creates a network object from the input file
				nw <- network(read.table(paste(input$rawdata[1,4])))
				if(input$fmh){
				  nw <- faux.mesa.high
				}
        set.network.attribute(nw, 'directed', input$dir)
        set.network.attribute(nw, 'hyper', input$hyper)
        set.network.attribute(nw, 'loops', input$loops)
        set.network.attribute(nw, 'bipartite', input$bipartite)
        set.network.attribute(nw, 'multiple', input$multiplex)
        nw
			})

    nwname <- reactive({input$rawdata[1,1]})


    #number of nodes in nw
    nodes <- reactive({
				nwreac()$gal$n}) 
    #get coordinates to plot network with
    coords <- reactive({
				plot.network(nwreac())})
    
    #list of vertex attributes in nw
    attr <- reactive({
    	  attr <- c()
          if(!is.null(input$rawdata)){      
    		    attr<-list.vertex.attributes(nwreac())
          }
          attr
      })

    #don't allow "na" or "vertex.names" as vertex attributes in menus on fit tab
    menuattr <- reactive({
      menuattr <- attr()
      if(is.element("na",menuattr)){
        menuattr <- menuattr[-which("na"==menuattr)]
      }
      if(is.element("vertex.names",menuattr)){
        menuattr <- menuattr[-which("vertex.names"==menuattr)]
      }
      menuattr
    })
    
    #numeric attributes only (for size menu, etc.)
    numattr <- reactive({
        numattr <- c()
        if(!is.null(input$rawdata)){  
          for(i in 1:length(attr())){
            if(is.numeric(get.vertex.attribute(nwreac(),attr()[i]))){
              numattr <- append(numattr,attr()[i])
            } 
          }} 
        numattr})
  
      nodesize <- reactive({
        nw <- nwreac()
        #scale size of nodes onto range between .7 and 3.5
        minsize <- min(get.vertex.attribute(nw,input$sizeby))
        maxsize <- max(get.vertex.attribute(nw,input$sizeby))
        if (input$sizeby == '1'){
          size = 1
        } else { 
          size = (get.vertex.attribute(nw,input$sizeby)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
        }
        size})

    legendlabels <- reactive({
      nw <- nwreac()
        if(input$colorby == 2){
          legendlabels <- NULL
        }else{
          legendlabels <- sort(unique(get.vertex.attribute(nw, input$colorby)))
          if(is.element("Other", legendlabels)){
            legendlabels <- legendlabels[-which(legendlabels=="Other")]
            legendlabels <- c(legendlabels, "Other")
          }
        }
        legendlabels
      })

    legendfill <- reactive({
      if(input$colorby == 2){
        legendfill <- NULL
      } else {
        legendfill <- as.color(legendlabels())
      }
      legendfill
    })

    
#' Some ergm terms (e.g. `gwesp`, `degree` and `nodematch`) take in their own arguments. 
#' The following reactive expressions take user input and create vectors that can later
#' be used as terms in an ergm formula.
#+ eval=FALSE    
    absdiff.terms <- reactive({
        aterms <- paste("absdiff('",input$chooseabsdiff,"', pow=",
                        input$absdiffpow,")", sep="")
      if(!any(input$terms == 'absdiff')){
        aterms <- NULL
      }
      aterms
    })

    gwesp.terms <- reactive({
      gterms <- paste("gwesp(",input$choosegwesp,
                      ", fixed = ",input$fixgwesp,")", sep="")
      if (!any(input$terms == 'gwesp')){
        gterms <- NULL
      }
      gterms})

    b1degree.terms <- reactive({
      bterms <- paste("b1degree(",input$chooseb1degree,")", sep="")
      if(input$chooseb1degree2 != ''){
        bterms <- paste("b1degree(c(",input$chooseb1degree2,"))", sep="")
      }
      if(!any(input$terms == 'b1degree')){
        bterms <- NULL
      }
      bterms})

    b2degree.terms <- reactive({
      bterms <- paste("b2degree(",input$chooseb2degree,")", sep="")
      if(input$chooseb2degree2 != ''){
        bterms <- paste("b2degree(c(",input$chooseb2degree2,"))", sep="")
      }
      if(!any(input$terms == 'b2degree')){
        bterms <- NULL
      }
      bterms})
    
    degree.terms <- reactive({
      dterms <- paste("degree(",input$choosedegree,")", sep="")
      if(input$choosedegree2 != ''){
        dterms <- paste("degree(c(",input$choosedegree2,"))", sep="")
      }
      if(!any(input$terms == 'degree')){
        dterms <- NULL
      }
      dterms})

    idegree.terms <- reactive({
      dterms <- paste("idegree(",input$chooseidegree,")", sep="")
      if(input$chooseidegree2 != ''){
        dterms <- paste("idegree(c(",input$chooseidegree2,"))", sep="")
      }
      if(!any(input$terms == 'idegree')){
        dterms <- NULL
      }
      dterms})

    odegree.terms <- reactive({
      dterms <- paste("odegree(",input$chooseodegree,")", sep="")
      if(input$chooseodegree2 != ''){
        dterms <- paste("odegree(c(",input$chooseodegree2,"))", sep="")
      }
      if(!any(input$terms == 'odegree')){
        dterms <- NULL
      }
      dterms})

    nodefactor.terms <- reactive({
      if(input$nodefactorbase ==''){
        nterms <- paste("nodefactor('",input$choosenodefactor,"')", sep="")
      } else {
        nterms <- paste("nodefactor('",input$choosenodefactor,"', base=",
                        input$nodefactorbase,")", sep="")
      }
      if(!any(input$terms == 'nodefactor')){
        nterms <- NULL
      }
      nterms
    })
    
    nodematch.terms <- reactive({
      middle <- paste(input$choosenodematch, collapse="', '")
      if(input$nodematchkeep == ''){
        nterms <- paste("nodematch('",input$choosenodematch,"', diff=", 
                        input$nodematchdiff,")", sep="")
      } else {
        nterms <- paste("nodematch('",input$choosenodematch,"', diff=", 
                        input$nodematchdiff,", keep=",input$nodematchkeep,")", sep="")
      }
      if(!any(input$terms == 'nodematch')){
        nterms <- NULL
      }
      nterms})
    
    nodemix.terms <- reactive({
      middle <- paste(input$choosenodemix, collapse="', '")
      if(input$nodemixbase == ''){
        nterms <- paste("nodemix(c('",middle,
                        "'))",sep="")
      } else {
        nterms <- paste("nodemix(c('",middle,
                        "'), base=",input$nodemixbase,")",sep="")
      }
      if(!any(input$terms == 'nodemix')){
        nterms <- NULL
      }
      nterms
    })

    nodecov.terms <- reactive({
        nterms <- paste("nodecov('",input$choosenodecov,
                        "')",sep="")
      if(!any(input$terms == 'nodecov')){
        nterms <- NULL
      }
      nterms
    })


#' `ergm.terms` is a compilation of all the terms entered,
#' which we then use to create a complete formula. 
#' 
#+ eval=FALSE    
    ergm.terms <- reactive({
      interms <- input$terms
      #all terms with extra menus associated
      menuterms <- c('absdiff', 'gwesp', 'degree', 'idegree', 'odegree', 'nodecov',
                     'nodematch', 'nodemix', 'nodefactor', 'b1degree', 'b2degree')
      #remove terms from formula if they are already counted with their menu options
      for(i in 1:length(menuterms)){
        if(any(interms == menuterms[i])){
          interms <- interms[-which(interms == menuterms[i])]
        }
      }
      paste(c(interms, absdiff.terms(), b1degree.terms(), b2degree.terms(),
              gwesp.terms(), degree.terms(), idegree.terms(), odegree.terms(),
              nodecov.terms(), nodefactor.terms(), nodematch.terms(), 
              nodemix.terms()), sep = '', collapse = '+')
      })
    
    ergm.formula <- reactive({
      formula(paste('nwreac() ~ ',ergm.terms(), sep = ''))})

#' Once we have a formula, creating a model object, checking the goodness of fit
#' and simulating from it is similar to what would be written in the command line,
#' wrapped in a reactive statement.
#+ eval=FALSE 
    model1reac <- reactive({
      if(input$fitButton == 0){
        return()
      }
      isolate(ergm(ergm.formula()))})
    
    #use default gof formula
    model1gof <- reactive({
      input$gofButton
      if(input$gofterm == ''){
        model1gof <- gof(model1reac())
      } else {
        gofform <- formula(paste('model1reac() ~ ', input$gofterm, sep = ''))
        model1gof <- gof(gofform)
      }
      isolate(model1gof)})

    model1mcmcdiag <- reactive({
      mcmc.diagnostics(model1reac())
    })
    

    model1simreac <- reactive({
      input$simButton
      isolate(simulate(model1reac(), nsim = input$nsims))})
    
#' Currently, the reactive statements that control the sizing/coloring/legend in the 
#' simulation plots use the attributes from the original network as a point of reference.
#' If the method for simulating networks changes from applying the same distribution of
#' attributes, these `get.vertex.attribute` commands for `minsize` and `maxsize`
#' would also need to change.
#+ eval=FALSE

    #get coordinates to plot simulations with
    sim.coords.1 <- reactive({
      input$simButton
      isolate(plot.network(model1simreac()))})
    sim.coords.2 <- reactive({
      plot.network(model1simreac()[[input$thissim]])})

    nodesize2 <- reactive({
      nw <- nwreac()
      #scale size of nodes onto range between .7 and 3.5
      
      minsize <- min(get.vertex.attribute(nw,input$sizeby2))
      maxsize <- max(get.vertex.attribute(nw,input$sizeby2))
      if (input$sizeby2 == '1'){
        size = 1
      } else { 
        if(input$nsims==1){
        size = (get.vertex.attribute(model1simreac(),input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
      }else{
        size = (get.vertex.attribute(model1simreac()[[input$thissim]],input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
      }}
      size})
    
    legendlabels2 <- reactive({
      nw <- nwreac()
      if(input$colorby2 == 2){
        legendlabels <- NULL
      }else{
        legendlabels <- sort(unique(get.vertex.attribute(nw, input$colorby2)))
        if(is.element("Other", legendlabels)){
          legendlabels <- legendlabels[-which(legendlabels=="Other")]
          legendlabels <- c(legendlabels, "Other")
        }
      }
      legendlabels
    })
    
    legendfill2 <- reactive({
      if(input$colorby2 == 2){
        legendfill <- NULL
      } else {
        legendfill <- as.color(legendlabels2())
      }
      legendfill
    })
    

#' Output Expressions
#' ---------------------------
#' Every piece of content that gets displayed in the app has to be
#' rendered by the appropriate `render*` function, e.g. `renderPrint` for text 
#' and `renderPlot` for plots. Most of the render functions here call 
#' reactive objects that were created above. I have divided the output objects
#' into sections depending on what tab of the app they are called from.
#'  
#' **Data Upload**
#' 
#+ eval=FALSE


output$rawdata <- renderPrint({
  raw <- matrix(nrow=3,ncol=1)
  rownames(raw)<-c("name:", "size:", "type:")
  if(!is.null(input$rawdata)){
    raw[1,1] <- input$rawdata[1,1]
    raw[2,1] <- paste(input$rawdata[1,2], " bytes")
    if(input$rawdata[1,3]==""){
      raw[3,1] <- "unknown"
    } else {
      raw[3,1] <- input$rawdata[1,3]
    }
  }
  write.table(raw, quote=FALSE, col.names=FALSE)})

#summary of network attributes
output$attr <- renderPrint({
  if (is.null(input$rawdata)){
    return(cat('NA'))
  }
  nw <- nwreac()
  return(nw)
})

output$changedir <- renderUI({
  if(is.null(input$rawdata)){
    return()
  }
  checkboxInput('dir', 'directed?', value=is.directed(nwinit()))
})
output$changehyper <- renderUI({
  if(is.null(input$rawdata)){
    return()
  }
  checkboxInput('hyper', 'hyper?', value=is.hyper(nwinit()))
})
output$changeloops <- renderUI({
  if(is.null(input$rawdata)){
    return()
  }
  checkboxInput('loops', 'loops?', value=has.loops(nwinit()))
})
output$changemultiplex <- renderUI({
  if(is.null(input$rawdata)){
    return()
  }
  checkboxInput('multiplex', 'multiplex?', value=is.multiplex(nwinit()))
})
output$changebipartite <- renderUI({
  if(is.null(input$rawdata)){
    return()
  }
  checkboxInput('bipartite', 'bipartite?', value=is.bipartite(nwinit()))
})

#' **Network Plots** 
#' 
#' Because the menu options for coloring/sizing the nodes on a network plot 
#' depend on which network has been selected, we have to dynamically render 
#' these input menus, rather than statically defining them in `ui.R`.  
#' *Note*, the dynamic widget object for the color menu has been assigned to
#' `output$dynamiccolor`, but when the user interacts with this menu, the input object
#' will still be saved in `input$colorby` because that is the widget inputId.
#' 
#+ eval=FALSE
    #summary of network attributes
    output$attr2 <- renderPrint({
      if (is.null(input$rawdata)){
        return(cat('NA'))
      }
      nw <- nwreac()
      return(nw)
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
#' The network plot takes display options from the sidebar of the ui. Even though 
#' I set the value of the 'None' option in the `sizeby` menu (above) as `1`, it gets
#' coerced into the string `'1'` by the rest of the strings in the vector of menu 
#' options. The variable `size` takes the value 1 if the user wants all the nodes
#' to be the same size, and otherwise maps the values of the numeric attributes into 
#' the range between .7 and 3.5 using the formula $y = (x-a)/(b-a) * (d-c) + c$, where
#' $x$ is the input in some range $[a,b]$ and $y$ is the output in range $[c,d]$.
#'
#+ eval=FALSE
    output$nwplot <- renderPlot({
      if (is.null(input$rawdata)){
        return()
      }
      nw <- nwreac()
      plot.network(nw, coord = coords(), 
                   displayisolates = input$iso, 
                   displaylabels = input$vnames, 
                   vertex.col = input$colorby,
                   vertex.cex = nodesize())
      if(input$colorby != 2){
        legend('bottomright', legend = legendlabels(), fill = legendfill())
      }
    })

    output$nwplotdownload <- downloadHandler(
      filename = function(){paste(nwname(),'_plot.pdf',sep='')},
      content = function(file){
        pdf(file=file, height=10, width=10)
        plot(nwreac(), coord = coords(), 
             displayisolates = input$iso, 
             displaylabels = input$vnames, 
             vertex.col = input$colorby,
             vertex.cex = nodesize())
        if(input$colorby != 2){
          legend('bottomright', legend = legendlabels(), fill = legendfill())
        }
        dev.off()
      }
      )

#DEGREE DISTRIBUTION

    output$dynamiccolor_dd <- renderUI({
      selectInput('colorby_dd',
                  label = 'Color bars according to:',
                  c('None', menuattr()),
                  selectize = FALSE)
    })

    dd_plotdata <- reactive({
      if(is.directed(nwreac())){
        gmode <- "digraph"
      } else {
        gmode <- "graph"
      }
      if(has.loops(nwreac())){
        diag <- TRUE
      } else {
        diag <- FALSE
      }
      data <-table(degree(nwreac(), gmode=gmode, cmode=input$cmode, diag=diag,
                          rescale=input$rescale))
      #for color-coded bars
      maxdeg <- max(as.numeric(names(table(degree(nwreac(), gmode=gmode)))))
      if(input$colorby_dd != "None"){
        if(is.directed(nwreac())){
          if(input$cmode=='indegree'){
            data <- summary(nwreac() ~ idegree(0:maxdeg, input$colorby_dd))
          } else if(input$cmode=='outdegree'){
            data <- summary(nwreac() ~ odegree(0:maxdeg, input$colorby_dd))
          } else {
            return('Cannot color code a directed graph using total degree.')
          }
        } else {
          data <- summary(nwreac() ~ degree(0:maxdeg, input$colorby_dd))
        }
        data <- t(matrix(data,nrow=maxdeg+1))
        colnames(data) <- 0:maxdeg
      }
      data
    })

    output$degreedist <- renderPlot({
      leg <- FALSE
      legtitle <- FALSE
      color <- "#3182bd"
      if(!is.null(input$colorby_dd)){
      if(input$colorby_dd != "None"){
        leg <- sort(unique(get.vertex.attribute(nwreac(),input$colorby_dd)))
        legtitle <- list(title = input$colorby_dd)
        color <- brewer.pal(dim(dd_plotdata())[1],"Blues")
      }}
      
      barplot(dd_plotdata(), xlab="Degree", legend.text=leg,
              args.legend=legtitle, col=color) 
    })

    output$degreedistdownload <- downloadHandler(
      filename = function(){paste(nwname(),'_degreedist.pdf',sep='')},
      content = function(file){
        pdf(file=file, height=10, width=10)
        leg <- FALSE
        legtitle <- FALSE
        color <- "#3182bd"
        if(!is.null(input$colorby_dd)){
          if(input$colorby_dd != "None"){
            leg <- sort(unique(get.vertex.attribute(nwreac(),input$colorby_dd)))
            legtitle <- list(title = input$colorby_dd)
            color <- brewer.pal(dim(dd_plotdata())[1],"Blues")
          }}
        
        barplot(dd_plotdata(), xlab="Degree", legend.text=leg,
                args.legend=legtitle, col=color) 
        dev.off()
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

    output$listofterms <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      if(nwreac()$gal$directed & nwreac()$gal$bipartite){
        current.terms <- intersect(dir.terms, bip.terms)
      } else if(nwreac()$gal$directed) {
        current.terms <- intersect(dir.terms, unip.terms)
      } else if(nwreac()$gal$bipartite){
        current.terms <- intersect(undir.terms, bip.terms)
      } else if(!nwreac()$gal$bipartite & !nwreac()$gal$bipartite){
        current.terms <- intersect(undir.terms, unip.terms)
      }
      selectInput('terms',label = 'Choose term(s):',
                  current.terms,
                  selected='edges',
                  multiple=TRUE, 
                  width = '4cm')
    })

    output$dynamicdegree <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('choosedegree', 
                  label = 'Choose degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
                  selected = 1,
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicb1degree <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('chooseb1degree',
                  label = 'Choose degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
                  selected = 1,
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicb2degree <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('chooseb2degree',
                  label = 'Choose degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
                  selected = 1,
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicidegree <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('chooseidegree',
                  label = 'Choose in-degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
                  selected = 1,
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicodegree <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('chooseodegree',
                  label = 'Choose out-degree(s)',
                  choices=paste(0:(as.numeric(nodes())-1)),
                  selected = 1,
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicabsdiff <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('chooseabsdiff',
                  label = 'Attribute for absdiff',
                  numattr(),
                  selected = numattr()[1],
                  multiple = TRUE,
                  width = '3cm')
    })

    output$dynamicnodefactor <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('choosenodefactor',
                  label = 'Attribute for nodefactor',
                  menuattr(),
                  selected = menuattr()[1],
                  multiple = TRUE,
                  width = '3cm')
    })
    
    output$dynamicnodematch <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('choosenodematch', 
                  label = 'Attribute for nodematch',
                  menuattr(),
                  selected = menuattr()[1],
                  multiple = TRUE,
                  width = '3cm')
    })
    output$dynamicnodemix <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('choosenodemix',
                  label = 'Attribute for nodemix',
                  menuattr(),
                  selected = menuattr()[1],
                  multiple = TRUE,
                  width = '3cm')
    })
    output$dynamicnodecov <- renderUI({
      if(is.null(input$rawdata)){
        return()
      }
      selectInput('choosenodecov',
                  label = 'Attribute for nodecov',
                  numattr(),
                  selected = numattr()[1],
                  multiple = TRUE,
                  width = '3cm')
    })

#' Below I output the current formulation of the ergm 
#' model so the user can clearly see how their menu selections change the model.
#' Since `ergm.terms()` is a reactive object, it will automatically update when
#' the user clicks on menu options.
#'  
#+ fitmodel2, eval=FALSE
    output$currentdataset1 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      cat(isolate(input$rawdata[1,1]))
    })

    output$checkterms1 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      cat(ergm.terms())
    })

    output$prefitsum <- renderPrint({
      if(is.null(input$rawdata) | length(input$terms)==0){
        return(cat('NA'))
      }
      options(width=150)
      summary(ergm.formula())
    })

    output$modelfit <- renderPrint({
      if (input$fitButton == 0){
        return(cat('Please choose term(s) for the model'))
      }
      model1reac()
    })

    output$modelfitsum <- renderPrint({
      if (input$fitButton == 0){
        return(cat('Please choose term(s) for the model'))
      }
      summary(model1reac())
    })

    #make sure that mcmc iterations output on the fitting tab
    outputOptions(output, "modelfit",priority=10, suspendWhenHidden=FALSE)
    outputOptions(output, "modelfitsum",priority=-10)
    

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
#' In the reactive section above the creation of `model1gof` depends on the term the 
#' user inputs. After checking that the user has already clicked the `actionButton`
#' on the page we can output the text of the gof object and the plot of the gof object.
#+ eval=FALSE
    #dataset only updates after goButton on first tab has been clicked
    output$currentdataset2 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      cat(nwname())
    })
    
    #formula only updates after fitButton has been clicked
    output$checkterms2 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      if(input$fitButton == 0){
        return(cat('Please fit a model'))
      }
      cat(isolate(ergm.terms()))
    })
    
    output$gofsummary <- renderPrint({
      if (input$gofButton == 0){
        return()
      }
      
      return(isolate(model1gof()))
      })
    
    
    output$gofplot <- renderPlot({   
      gofterm <- isolate(input$gofterm)
      if (gofterm == ''){
        par(mfrow=c(3,1))
      } else {
        par(mfrow=c(1,1))
      }
      
      isolate(plot.gofobject(model1gof()))
      par(mfrow=c(1,1))
    })

    output$gofplotdownload <- downloadHandler(
      filename = function(){paste(nwname(),'_gof.pdf',sep='')},
      content = function(file){
        if(input$gofterm == ''){
          par(mfrow=c(3,1))
        } else {
          par(mfrow=c(1,1))
        }
        pdf(file=file, height=4, width=10)
        plot.gofobject(model1gof())
        par(mfrow=c(1,1))
        dev.off()
      }
    )

    output$gofplotspace <- renderUI({
      if(input$gofButton == 0){
        return()
      }
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
#' diagnostics and plots all output together. Instead of calling `mcmc.diagnositcs`
#' a reactive object, .
#' 
#+ eval=FALSE

    output$checkterms3 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      if(input$fitButton == 0){
        return(cat('Please fit a model'))
      }
      cat(isolate(ergm.terms()))
    })
    output$currentdataset3 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      cat(nwname())
    })
    
    output$diagnosticsplot <- renderPlot({
      vpp <- length(model1reac()$coef)
      mcmc.diagnostics(model1reac(), vars.per.page = vpp)
    })

    output$mcmcplotdownload <- downloadHandler(
      vpp <- length(model1reac()$coef),
      filename = function(){paste(nwname(),'_mcmc.pdf',sep='')},
      content = function(file){
        pdf(file=file, height=vpp*4/3, width=10)
        mcmc.diagnostics(model1reac(), vars.per.page = vpp)
        dev.off()
      }
    )

    output$diagnosticsplotspace <- renderUI({
      if(input$fitButton == 0){
        return()
      }
      vpp <- length(model1reac()$coef)
      plotOutput('diagnosticsplot', height = vpp*400/3)
    })
    
    output$diagnostics <- renderPrint({
        if(input$fitButton == 0){
          return()
        }
        isolate(mcmc.diagnostics(model1reac()))

    })


#' **Simulations**
#' 
#' On this page the user can choose how many simulations of the model to run. The 
#' reactive object `model1simreac` contains all the simulations, which we can output
#' a summary of and choose one simulation at a time to plot. *Note:* when the user
#' chooses to simulate one network, `model1simreac()` is a reactive object of class
#' network. When the user chooses to simulate multiple networks, `model1simreac()`
#' contains a list of the generated networks. This is why we have to split up the plot
#' command in an if-statement. The rest of the display options should look familiar
#' from the 'Plot Network' tab.
#+ eval=FALSE
    output$checkterms4 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      if(input$fitButton == 0){
        return(cat('Please fit a model'))
      }
      cat(isolate(ergm.terms()))
    })
    output$currentdataset4 <- renderPrint({
      if(is.null(input$rawdata)){
        return(cat('Upload a dataset'))
      }
      cat(nwname())
    })
    

    output$sim.summary <- renderPrint({
      if (input$simButton == 0){
        return(cat(''))
      }
      model1sim <- isolate(model1simreac())
      if (isolate(input$nsims) == 1){
        return(model1sim)
      }
      return(summary(model1sim))
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
      nw <- nwreac()
      nsims <- isolate(input$nsims)
      model1sim <- isolate(model1simreac()) 
      
      #can't plot simulation number greater than total sims
      if(input$thissim > nsims){
        return()
      } 
      
      if (nsims == 1){
        
        plot(model1sim, coord = sim.coords.1(), 
             displayisolates = input$iso2, 
             displaylabels = input$vnames2, 
             vertex.col = input$colorby2,
             vertex.cex = nodesize2())
        if(input$colorby2 != 2){
          legend('bottomright', legend = legendlabels2(), fill = legendfill2())
        }
      } else {
        plot(model1sim[[input$thissim]], 
             coord = sim.coords.2(),
             displayisolates = input$iso2, 
             displaylabels = input$vnames2, 
             vertex.col = input$colorby2,
             vertex.cex = nodesize2())
        if(input$colorby2 != 2){
          legend('bottomright', legend = legendlabels2(), fill = legendfill2())
        }
      }
    })

    output$simplotdownload <- downloadHandler(
      filename = function(){paste(nwname(),'_simplot.pdf',sep='')},
      content = function(file){
        pdf(file=file, height=10, width=10)
        if(input$nsims == 1){
        plot(model1simreac(), 
             coord = sim.coords.1(), 
             displayisolates = input$iso2, 
             displaylabels = input$vnames2, 
             vertex.col = input$colorby2,
             vertex.cex = nodesize2())
        }else{
          plot(model1simreac()[[input$thissim]], 
               coord = sim.coords.2(), 
               displayisolates = input$iso2, 
               displaylabels = input$vnames2, 
               vertex.col = input$colorby2,
               vertex.cex = nodesize2())
        }
        if(input$colorby2 != 2){
          legend('bottomright', legend = legendlabels2(), fill = legendfill2())
        }
        dev.off()
      }
    )
    
    
  })

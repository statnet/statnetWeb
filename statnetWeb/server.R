#' ---
#' title: "statnetWeb, server.R"
#' author: "Emily Beylerian"
#' ---
#' statnetWeb
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
library(shinyBS)
library(statnet)
library(RColorBrewer)
library(lattice)
library(latticeExtra)
#source("chooser.R")


#' Loading data and assigning variables outside of the call to `shinyServer`
#' saves time because this code will not get re-run.These don't depend on any user input
#' and will never change value, so they can be global variables (common to all
#' shiny sessions). 
#+ eval=FALSE 

data(faux.mesa.high)
data(faux.magnolia.high)
data(florentine)
data(sampson)
data(samplk)
data(ecoli)
data(molecule)
data(kapferer)

options(digits=3)

#create a list of unique term names
# UNCOMMENT AFTER RELEASE FOR TERM DOCUMENTATION
# allterms <- search.ergmTerms()
# inds <- gregexpr(pattern='\\(', allterms)
# for(i in 1:length(allterms)){
#   allterms[i] <- substr(allterms[[i]], start=1, stop=inds[[i]][1]-1)
# }
# allterms <- unique(allterms)

   


shinyServer(
  function(input, output, session){
    
assign('v_attrNamesToAdd', list(1),
       pos="package:base")
assign('v_attrValsToAdd', list(),  
       pos="package:base")
assign('e_attrNamesToAdd', list(1),
       pos="package:base" )
assign('e_attrValsToAdd', list(),  
       pos="package:base")
assign('ev_attrNamesToAdd', list(1),
       pos="package:base" )
assign('ev_attrValsToAdd', list(),  
       pos="package:base")

assign('input_termslist', list(),
       pos="package:base")

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

#this reactive expression is only to get the initial values of the network
nwinit <- reactive({
  #input$rawdatafile comes as a dataframe with name, size, type and datapath
  #datapath is stored in 4th column of dataframe
  #network creates a network object from the input file
  if(is.null(input$rawdatafile)){
    nw <- NULL
  } else {
    filepath <- input$rawdatafile[1,4]
    filename <- input$rawdatafile[1,1]
  }
  if(input$filetype == 1){
    if(!is.null(input$rawdatafile)){
      nw <- tryCatch({
        obj <- load(paste(filepath))
      }, error = function(err){
        return("Chosen file is not an R object")
      }, finally = NULL
      )
      try(nw <- get(obj))
    }
  } else if(input$filetype == 2){
    if(!is.null(input$rawdatafile)){
      nw <- "Upload a .net file"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".net" |
           substr(filename,nchar(filename)-3,nchar(filename))==".NET"){
        nw <- read.paj(paste(filepath))
      }
    }
  } else if(input$filetype == 3){
    if(!is.null(input$rawdatafile)){
      nw <- "Upload a .paj file"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".paj" |
           substr(filename,nchar(filename)-3,nchar(filename))==".PAJ"){
        nws <- read.paj(paste(filepath))
        if(!is.null(pajnws())){
          nw <- nws$networks[[as.numeric(input$choosepajnw)]]
        }
      }
    }
  } else if(input$filetype == 4){
    if(!is.null(input$rawdatafile)){
      nw <- "Input the specified type of matrix"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
        header <- TRUE
        row.names<-1
        if(input$matrixtype == "edgelist"){
          header <- FALSE
          row.names<-NULL
        }
        try(nw <- network(read.csv(paste(filepath), sep=",", header=header, row.names=row.names),
                          directed=input$dir, loops=input$loops,
                          multiple=input$multiple, bipartite=input$bipartite,
                          matrix.type=input$matrixtype,
                          ignore.eval=FALSE, names.eval='edgevalue'))
      }
      try(nw <- network(read.table(paste(filepath)),
                        directed=input$dir, loops=input$loops,
                        multiple=input$multiple, bipartite=input$bipartite,
                        matrix.type=input$matrixtype,
                        ignore.eval=FALSE, names.eval='edgevalue'))
    }
    
  } else if(input$filetype ==5){
    if(input$samplenet == "None"){
      nw <- NULL
    } else {
      nw <- eval(parse(text = input$samplenet))
      if(!is.element('bipartite',names(nw$gal))){
        set.network.attribute(nw,'bipartite',FALSE)
      }
    }
  }

  nw
})

#number of nodes in nw
nodes <- reactive({
  if(!is.network(nwinit())){return()}
  nwinit()$gal$n
})

#number of edges in nw
nedges <- reactive({
  if(!is.network(nwinit())) return()
  network.edgecount(nwinit())
})

#initial vertex attributes
vattrinit <- reactive({
  vattrinit <- c()
  if(is.network(nwinit())){        
    vattrinit<-list.vertex.attributes(nwinit())
  }
  vattrinit
})

#matrix of vertex attribute values
vattrinit.vals <- reactive({
  m <- matrix(nrow=nodes(),ncol=length(vattrinit()))
  for(j in 1:length(vattrinit())){
    m[,j]<-get.vertex.attribute(nwinit(),vattrinit()[j])
  }
  m
})

#set correct number of rows for the value lists, 
#so that we can add columns later
observe({
  nwinit()
  #reset lists when uploaded network changes
  vdf <- list()
  edf <- list()
  evdf <- list()
  if (is.network(nwinit())){
    n <- nodes()
    e <- nedges()
    for (i in 1:n){
      vdf <- rbind(vdf,i)
    }
    for (i in 1:e){
      edf <- rbind(edf,i)
      evdf <- rbind(evdf,i)
    }
    assign("v_attrValsToAdd", vdf, pos="package:base")
    assign("e_attrValsToAdd", edf, pos="package:base")
    assign("ev_attrValsToAdd", evdf, pos="package:base")
    assign('v_attrNamesToAdd', list(1),
           pos="package:base" )
    assign('e_attrNamesToAdd', list(1),
           pos="package:base" )
    assign('ev_attrNamesToAdd', list(1),
           pos="package:base" )
    
    assign('vertex_names', network.vertex.names(nwinit()), pos="package:base")
    
  }
})

#' TO KEEP LIST OF ALL NEW ATTRIBUTES FROM USER
#' can't use global variables because they are common to all 
#' sessions using the app, instead created per session variables 
#' inside shinyServer
#' 
#' 
#+ eval=FALSE

newattrnamereac <- reactive({
  newname <- ''
  try({
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]

    if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
      newattrs <- read.csv(paste(path), sep=",", header=TRUE, stringsAsFactors=FALSE)
      newname <- names(newattrs)
    } else {
      objname <- load(paste(path))
      newattrs <- get(objname)
      newname <- names(newattrs)
    }
    
  })
  newname
})

#save vertex names
observe({
  if(input$newattrButton == 0) return()
  isolate({
    if(input$newattrtype == 'vertex names'){
      path <- input$newattrvalue[1,4]
      filename <- input$newattrvalue[1,1]
      
      if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
        newnames <- read.csv(paste(path), sep=",", header=TRUE, stringsAsFactors=FALSE)
      } else {
        objname <- load(paste(path))
        newnames <- get(objname)
      }
      
      assign('vertex_names', newnames, pos='package:base')
    }
  })
})

#add vertex attributes to list
observe({
  if(input$newattrButton == 0) return()
  isolate({
      if(input$newattrtype == 'vertex attribute'){
          path <- input$newattrvalue[1,4]
          filename <- input$newattrvalue[1,1]
          
          if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
            newattrs <- read.csv(paste(path), sep=",", header=TRUE, stringsAsFactors=FALSE)
            newname <- names(newattrs)
          } else {
            objname <- load(paste(path))
            newattrs <- get(objname)
            newname <- names(newattrs)
          }
          
          namesofar <- get("v_attrNamesToAdd", pos="package:base")
          valsofar <- get("v_attrValsToAdd", pos="package:base")
          for(k in 1:length(newname)){
            namesofar <- cbind(namesofar, newname[[k]])
            valsofar <- cbind(valsofar, newattrs[[k]])
          }
          
          assign('v_attrNamesToAdd', namesofar,
                 pos="package:base")
          assign('v_attrValsToAdd', valsofar,
                 pos="package:base")
        }
  })
})

#add edge attributes to list
observe({
  if(input$newattrButton == 0) return()
  isolate({
    if(input$newattrtype == 'edge attribute'){
      path <- input$newattrvalue[1,4]
      filename <- input$newattrvalue[1,1]
      
      if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
        newattrs <- read.csv(paste(path), sep=",", header=TRUE, stringsAsFactors=FALSE)
        newname <- names(newattrs)
      } else {
        objname <- load(paste(path))
        newattrs <- get(objname)
        newname <- names(newattrs)
      }

      namesofar <- get("e_attrNamesToAdd", pos="package:base")
      valsofar <- get("e_attrValsToAdd", pos="package:base")
      for(k in 1:length(newname)){
        namesofar <- cbind(namesofar, newname[[k]])
        valsofar <- cbind(valsofar, newattrs[[k]])
      }
        assign('e_attrNamesToAdd', namesofar,
               pos="package:base")
        assign('e_attrValsToAdd', valsofar,
               pos="package:base")
      }
  })
})

#add edge values to list
observe({
  if(input$newattrButton == 0) return()
  isolate({
    if(input$newattrtype == 'edge value'){
      path <- input$newattrvalue[1,4]
      filename <- input$newattrvalue[1,1]
      
      if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
        newattrs <- read.csv(paste(path), sep=",", header=TRUE, stringsAsFactors=FALSE)
        newname <- names(newattrs)
      } else {
        objname <- load(paste(path))
        newattrs <- get(objname)
        newname <- names(newattrs)
      }
      namesofar <- get("ev_attrNamesToAdd", pos="package:base")
      valsofar <- get("ev_attrValsToAdd", pos="package:base")
      j <- length(valsofar)
      for(k in 1:length(newname)){
        namesofar <- cbind(namesofar, newname[[k]])
        valsofar[[j+k]] <- newattrs[[k]]
      }
      assign('ev_attrNamesToAdd', namesofar,
             pos="package:base")
      assign('ev_attrValsToAdd', valsofar,
             pos="package:base")
    }
  })
})

#attributes will be added to this network
nwmid <- reactive({
    nw <- nwinit()
  
    if (class(nw)=="network"){
      #preserve initial network attributes and let user choose if directed 
      #after symmetrizing
      if(input$symmetrize != "Do not symmetrize"){
        if(input$aftersymm == 'directed'){
          nw <- network(symmetrize(nw, rule=input$symmetrize), directed=TRUE,
                        hyper=nwattrinit()[2], loops=nwattrinit()[3],
                        multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
        } else {
          nw <- network(symmetrize(nw, rule=input$symmetrize), directed=FALSE,
                        hyper=nwattrinit()[2], loops=nwattrinit()[3],
                        multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
        }
        #add initial vertex attributes back after symmetrizing
        #can't add edge attributes back because number of edges has changed
        for(k in 1:length(vattrinit())){
          attr_names <- vattrinit()
          attr_matrix <- vattrinit.vals()
          set.vertex.attribute(nw,attr_names[k],attr_matrix[,k])
        }
      }
      
      v_attrNamesToAdd <- get('v_attrNamesToAdd',pos='package:base')
      v_attrValsToAdd <- get('v_attrValsToAdd', pos='package:base')
      e_attrNamesToAdd <- get('e_attrNamesToAdd',pos='package:base')
      e_attrValsToAdd <- get('e_attrValsToAdd', pos='package:base')
      ev_attrNamesToAdd <- get('ev_attrNamesToAdd',pos='package:base')
      ev_attrValsToAdd <- get('ev_attrValsToAdd', pos='package:base')
      
      
      input$newattrButton
      try({
        vertex_names <- get('vertex_names', pos='package:base')
        network.vertex.names(nw) <- vertex_names
      })
      v_numnew <- length(v_attrNamesToAdd)
      if(v_numnew > 1){
        for (j in 2:v_numnew){
          v_newname <- as.character(v_attrNamesToAdd[1,j])
          v_newval <- v_attrValsToAdd[,j]
          set.vertex.attribute(nw,v_newname,v_newval)
        }
      }
      
      e_numnew <- length(e_attrNamesToAdd)
      if(e_numnew > 1){
        for (k in 2:e_numnew){
          e_newname <- as.character(e_attrNamesToAdd[1,k])
          e_newval <- e_attrValsToAdd[,k]
          set.edge.attribute(nw,e_newname,e_newval)
        }
      }
      
      ev_numnew <- length(ev_attrNamesToAdd)
      if(ev_numnew > 1){
        for (l in 2:ev_numnew){
          ev_newname <- as.character(ev_attrNamesToAdd[1,l])
          ev_newval <- ev_attrValsToAdd[[l]]
          set.edge.value(nw,ev_newname,ev_newval)
        }
      }
    }
  
    nw
	})

#delete unwanted attributes from this network and use it for future
#calculations
network <- reactive({
  nw <- nwmid()
  
#   deleteme <- input$deleteattrs$right
#   len <- length(deleteme)
#   if(len>=1){
#       for(i in 1:len){
#         if(any(list.vertex.attributes(nw)==deleteme[i])){
#           delete.vertex.attribute(nw,deleteme[i])
#         }
#         if(any(list.edge.attributes(nw)==deleteme[i])){
#           delete.edge.attribute(nw,deleteme[i])
#         }
#       }
#   }
  nw
})


#list of everything in the Pajek project
pajnws <- reactive({
  nws <- NULL
  if((input$filetype == 3) & (!is.null(input$rawdatafile))){
    filename <- input$rawdatafile[1,1]
    if(substr(filename,nchar(filename)-3,nchar(filename))==".paj"){
    nws <- read.paj(paste(input$rawdatafile[1,4]))
    }
  }
  nws
})

nwname <- reactive({
  name <- input$rawdatafile[1,1]
  if(input$filetype == 5){
    name <- input$samplenet
  }
  name
  })

#get coordinates to plot network with
coords <- reactive({
		plot.network(network())})

#initial network attributes
#returns vector of true/falses
nwattrinit <- reactive({
  if(!is.network(nwinit())){return()}
  nwattributes <- c('directed','hyper','loops','multiple','bipartite')
  unlist(lapply(nwattributes,get.network.attribute,x=nwinit()))
})

#list of all vertex attributes in nw (after adding new)
attr <- reactive({
	  attr <- c()
    if(is.network(network())){        
		    attr<-list.vertex.attributes(network())
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
    if(is.network(network())){  
      for(i in 1:length(attr())){
        if(is.numeric(get.vertex.attribute(network(),attr()[i]))){
          numattr <- append(numattr,attr()[i])
        } 
      }} 
    numattr})

nodebetw <- reactive({
  if(!is.network(network())){return()}
  if(is.directed(network())){
    gmode <- 'digraph'
    cmode <- 'directed'
  } else {
    gmode <- 'graph'
    cmode <- 'undirected'
  }
  betweenness(network(), gmode=gmode, diag=has.loops(network()),
              cmode=cmode)
})

nodesize <- reactive({
  if(!is.network(network())){return()}
  nw <- network()
  #scale size of nodes onto range between .7 and 3.5
  minsize <- min(get.vertex.attribute(nw,input$sizeby))
  maxsize <- max(get.vertex.attribute(nw,input$sizeby))
  if (input$sizeby == '1'){
    size = 1.2
  } else if (input$sizeby == 'Betweenness'){
    minsize <- min(nodebetw())
    maxsize <- max(nodebetw())
    size = (nodebetw()-minsize)/(maxsize-minsize)*(3.5-.7)+.7
  } else {
    minsize <- min(get.vertex.attribute(nw,input$sizeby))
    maxsize <- max(get.vertex.attribute(nw,input$sizeby))
    size = (get.vertex.attribute(nw,input$sizeby)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }
  size})

vcol <- reactive({
  if(!is.network(network())){return()}
  nw <- network()
  if(input$colorby ==2){
    vcol <- 2
  } else {
    full_list <- get.vertex.attribute(nw,input$colorby)
    short_list <- sort(unique(full_list))
    if(is.element("Other", short_list)){ #to be consistent with order of legend
      short_list <- short_list[-which(short_list=="Other")]
      short_list <- c(short_list, "Other")
    }
    full_list <- match(full_list, short_list) 
    #each elt corresponds to integer position in short_list
    if(length(short_list)>9){
      full_list <- full_list %% 9
      full_list[full_list == 0] <- 9
    }
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    assigncolor <- function(x){
      switch(x, pal[1], pal[2], pal[3], pal[4], pal[5],
             pal[6], pal[7], pal[8], pal[9])
    }
    vcol <- sapply(X = full_list, FUN = assigncolor)
  }
  vcol
})

legendlabels <- reactive({
  if(!is.network(network())){return()}
  nw <- network()
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
    n <- length(legendlabels())
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    legendfill <- adjustcolor(pal, alpha.f = input$transp)
  }
  legendfill
})


#' `ergm.terms` is a compilation of all the terms entered,
#' which we then use to create a complete formula. 
#' 
#+ eval=FALSE 

#add terms to list as user enters them
observe({
  if(input$addtermButton==0) return()
  isolate({
    valsofar <- get('input_termslist',pos='package:base')
    newval <- input$terms
    assign('input_termslist', rbind(valsofar, newval),
           pos='package:base')
    updateTextInput(session, inputId='terms', value='')
  })
})

observe({
  if(input$resetformulaButton==0) return()
  isolate({
    assign('input_termslist', list(), pos='package:base')
    updateTextInput(session, inputId='terms', value='')
  })
})

ergm.terms <- reactive({
  input$resetformulaButton
  input$addtermButton
  interms <- get('input_termslist', pos='package:base')
  if(length(interms)==0) return('NA')
  paste(interms, collapse = '+')
  })

ergm.formula <- reactive({
  if(ergm.terms()=='NA')return()
  formula(paste('network() ~ ',ergm.terms(), sep = ''))})

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

nodebetw2 <- reactive({
  if(input$nsims==1){
    if(is.directed(model1simreac())){
      gmode <- "digraph"
      cmode <- "directed"
    } else {
      gmode <- "graph"
      cmode <- "undirected"
    }
    b <- betweenness(model1simreac(), gmode=gmode, cmode=cmode)
  } else {
    if(is.directed(model1simreac()[[input$thissim]])){
      gmode <- "digraph"
      cmode <- "directed"
    } else {
      gmode <- "graph"
      cmode <- "undirected"
    }
    b <- betweenness(model1simreac()[[input$thissim]], gmode=gmode, cmode=cmode)
  }
  b
})

nodesize2 <- reactive({
  nw <- network()
  #scale size of nodes onto range between .7 and 3.5
  if (input$sizeby2 == '1'){
    size = 1.2
  } else if (input$sizeby2 == "Betweenness"){
    minsize <- min(nodebetw2())
    maxsize <- max(nodebetw2())
    size <- (nodebetw2()-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  } else {
    minsize <- min(get.vertex.attribute(nw,input$sizeby2))
    maxsize <- max(get.vertex.attribute(nw,input$sizeby2))
    if(input$nsims==1){
    size <- (get.vertex.attribute(model1simreac(),input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }else{
    size <- (get.vertex.attribute(model1simreac()[[input$thissim]],input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }}
  size})

vcol2 <- reactive({
  if(!is.network(network())){return()}
  input$simButton
  isolate(nsim <- input$nsims)
  if(input$colorby2 ==2){
    vcol <- 2
  } else {
    if(nsim == 1){
      full_list <- get.vertex.attribute(model1simreac(),input$colorby2)
    } else {
      full_list <- get.vertex.attribute(model1simreac()[[input$thissim]],input$colorby2)
    }
    short_list <- sort(unique(full_list))
    if(is.element("Other", short_list)){ #to be consistent with order of legend
      short_list <- short_list[-which(short_list=="Other")]
      short_list <- c(short_list, "Other")
    }
    full_list <- match(full_list, short_list) 
    #each elt is an integer position in short_list
    if(length(short_list)>9){
      full_list <- full_list %% 9
      full_list[full_list==0] <- 9
    }
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    assigncolor <- function(x){
      switch(x, pal[1], pal[2], pal[3], pal[4], pal[5],
             pal[6], pal[7], pal[8], pal[9])
    }
    vcol <- sapply(X = full_list, FUN = assigncolor)
  }
  vcol
})

legendlabels2 <- reactive({
  nw <- network()
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
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    legendfill <- adjustcolor(pal, alpha.f = input$transp2)
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


output$rawdatafile <- renderPrint({
  raw <- matrix(nrow=2,ncol=1)
  rownames(raw)<-c("name:", "size:")
  if(!is.null(input$rawdatafile)){
    raw[1,1] <- input$rawdatafile[1,1]
    raw[2,1] <- paste(input$rawdatafile[1,2], " bytes")
  }
  write.table(raw, quote=FALSE, col.names=FALSE)})

output$pajchooser <- renderUI({
  pajlist <- 'None'
  if(!is.null(pajnws())){
    pajlist <- 1:length(pajnws()$networks)
  names(pajlist) <- names(pajnws()$networks)
  }
  selectInput('choosepajnw', label='Upload a Pajek project file and choose a network from it',
              choices = pajlist, selectize=FALSE)
})

output$newattrname <- renderPrint({
  if(!is.null(input$newattrvalue)){
      cat(newattrnamereac())}
})

# output$modifyattrchooser <- renderUI({
#   if(!is.network(nwmid())) return()
#   vattr <- list.vertex.attributes(nwmid())
#   eattr <- list.edge.attributes(nwmid())
#   attrlist <- c(vattr, eattr)
#   selectInput('modifyattrs', label=NULL, choices=attrlist,
#               selectize=FALSE)
# })


#summary of network attributes
output$nwsum <- renderPrint({
  if (is.null(network())){
    return(cat('NA'))
  }
  nw <- network()
  if (class(nw)!="network"){
    return(cat(nw))
  }
  return(nw)
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
  if (!is.network(network())){
    return(cat('NA'))
  }
  nw <- network()
  return(nw)
})

output$dynamiccolor <- renderUI({
  selectInput('colorby',
              label = 'Color nodes according to:',
              c('None' = 2, attr()),
              selectize = FALSE)
})
outputOptions(output,'dynamiccolor',suspendWhenHidden=FALSE)

output$colorwarning <- renderUI({
  if(length(legendlabels())<10) return()
  span(tags$u('Warning:'), ' Colors get recycled for attributes with',
       'more than nine levels.', style='font-size:0.85em;')
})

output$dynamicsize <- renderUI({
  selectInput('sizeby',
              label = 'Size nodes according to:',
              c('None' = 1, 'Betweenness', numattr()),
              selectize = FALSE)
})
outputOptions(output,'dynamicsize',suspendWhenHidden=FALSE)

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
  if (!is.network(network())){
    return()
  }
  nw <- network()
  color <- adjustcolor(vcol(), alpha.f = input$transp)
  plot.network(nw, coord = coords(), 
               displayisolates = input$iso, 
               displaylabels = input$vnames, 
               vertex.col = color,
               vertex.cex = nodesize())
  if(input$colorby != 2){
    legend('bottomright', title = input$colorby, legend = legendlabels(), fill = legendfill())
  }
})

output$nwplotdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_plot.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=10)
    nw <- network()
    color <- adjustcolor(vcol(), alpha.f = input$transp)
    plot.network(nw, coord = coords(), 
                 displayisolates = input$iso, 
                 displaylabels = input$vnames, 
                 vertex.col = color,
                 vertex.cex = nodesize())
    if(input$colorby != 2){
      legend('bottomright', title=input$colorby, legend = legendlabels(), fill = legendfill())
    }
    dev.off()
  }
  )

#Data to use for null hypothesis overlays in network plots
uniformsamples <- reactive({
  if(!is.network(network())){
    return()
  }
  if(is.directed(network())){
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='digraph',
                    diag=has.loops(network()))
  } else {
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='graph',
                    diag=has.loops(network()))
  }
  samples
})

bernoullisamples <- reactive({
  if(!is.network(network())){
    return()
  }
  density <- gden(network())
  if(is.directed(network())){
    samples <- rgraph(n=nodes(), m=50, mode='digraph', tprob=density,
                      diag=has.loops(network()))
  } else {
    samples <- rgraph(n=nodes(), m=50, mode='graph', tprob=density,
                      diag=has.loops(network()))
  }
  samples
})

#DEGREE DISTRIBUTION

output$dynamiccolor_dd <- renderUI({
  selectInput('colorby_dd',
              label = 'Color bars according to:',
              c('None', menuattr()),
              selectize = FALSE)
})
outputOptions(output,'dynamiccolor_dd',suspendWhenHidden=FALSE)

dd_plotdata <- reactive({
  if(!is.network(network())){
    return()
  }
  if(is.directed(network())){
    gmode <- "digraph"
  } else {
    gmode <- "graph"
  }
  if(has.loops(network())){
    diag <- TRUE
  } else {
    diag <- FALSE
  }
  deg <- degree(network(), gmode=gmode, cmode=input$cmode, diag=diag)
  data <-tabulate(deg)
  data <- append(data,sum(deg==0),after=0)
  maxdeg <- max(deg)
  names(data) <- paste(0:maxdeg)
  
  #for color-coded bars
  if(!is.null(input$colorby_dd) & input$colorby_dd != "None"){
    if(is.directed(network())){
      if(input$cmode=='indegree'){
        data <- summary(network() ~ idegree(0:maxdeg, input$colorby_dd))
      } else if(input$cmode=='outdegree'){
        data <- summary(network() ~ odegree(0:maxdeg, input$colorby_dd))
      } else {
        return('Cannot color code a directed graph using total degree.')
      }
    } else {
      data <- summary(network() ~ degree(0:maxdeg, input$colorby_dd))
    }
    data <- t(matrix(data,nrow=maxdeg+1))
    colnames(data) <- 0:maxdeg
  }
  data
})

dd_uniformoverlay <- reactive({
  if(!is.network(network())){
    return()
  }
  reps <- 50 #number of draws
  if(is.directed(network())){
    deg <- degree(uniformsamples(), g=1:reps, gmode='digraph', cmode=input$cmode)
  } else {
    deg <- degree(uniformsamples(), g=1:reps, gmode='graph', cmode=input$cmode)
  }
    #now deg is a matrix where each element is a degree of a node 
    #each column is a different draw
  
  degreedata <- apply(deg, MARGIN=2, FUN=tabulate, nbins=max(deg))
    #degreedata is matrix holding tabulation of degrees (except isolates)
    #each column is different draw
  z <- apply(deg, MARGIN=2, FUN=function(x){sum(x==0)}) #tabulation of isolates
  degreedata <- rbind(z, degreedata) #complete tabulation for each draw
  degreemeans <- apply(degreedata, MARGIN=1, FUN=mean)
  names(degreemeans) <- paste(0:max(deg))
  degreesd <- apply(degreedata, MARGIN=1, FUN=sd)
  mean_and_sd <- list(degreemeans, degreesd)
})

dd_bernoullioverlay <- reactive({
  if(!is.network(network())){
    return()
  }
  reps = 50
  density <- gden(network())
  if(is.directed(network())){
    deg <- degree(bernoullisamples(), g=1:reps, gmode='digraph', cmode=input$cmode)
  } else {
    deg <- degree(bernoullisamples(), g=1:reps, gmode='graph', cmode=input$cmode)
  }
  #now deg is a matrix where each element is a degree of a node 
  #each column is a different draw
  
  degreedata <- apply(deg, MARGIN=2, FUN=tabulate, nbins=max(deg))
  #degreedata is matrix holding tabulation of degrees (except isolates)
  #each column is different draw
  z <- apply(deg, MARGIN=2, FUN=function(x){sum(x==0)})
  #tabulation of isolates in each draw
  degreedata <- matrix(data=c(z,t(degreedata)),nrow=max(deg)+1,ncol=reps, byrow=TRUE)
  #complete tabulation of degrees for each draw
  degreemeans <- apply(degreedata, MARGIN=1, FUN=mean)
  names(degreemeans) <- paste(0:max(deg))
  degreesd <- apply(degreedata, MARGIN=1, FUN=sd)
  mean_and_sd <- list(degreemeans, degreesd)
})

output$degreedist <- renderPlot({
  if(!is.network(network())){
    return()
  }
  plotme <- dd_plotdata()
  color <- "#79AED4"
  ylabel <- "Count of Nodes"
  ltext <- c()
  lcol <- c() #color for lines
  lty <- c()
  lpch <- c()
  lfill <- c() #color for boxes
  lborder <- c()
  ltitle <- NULL
  if(!is.null(input$colorby_dd)){
  if(input$colorby_dd != "None"){
    ncolors <- dim(dd_plotdata())[1]
    color <- brewer.pal(ncolors,"Blues")[1:ncolors]
    ltext <- sort(unique(get.vertex.attribute(network(),input$colorby_dd)))
    ltext <- append(ltext, "")
    lfill <- c(color, 0)
    lborder <- append(lborder, c(rep("black", times=ncolors), 0))
    lty <- rep(0, times=ncolors+1)
    lpch <- rep(26, times=ncolors+1)
    ltitle <- input$colorby_dd
  }}
  
  unif_samplemeans <- dd_uniformoverlay()[[1]]
  unif_stderr <- dd_uniformoverlay()[[2]]
  unif_upperline <- unif_samplemeans + 2*unif_stderr
  unif_lowerline <- unif_samplemeans - 2*unif_stderr
  maxdeg_u <- length(unif_samplemeans)-1
  
  bern_samplemeans <- dd_bernoullioverlay()[[1]]
  bern_stderr <- dd_bernoullioverlay()[[2]]
  bern_upperline <- bern_samplemeans + 2*bern_stderr
  bern_lowerline <- bern_samplemeans - 2*bern_stderr
  maxdeg_b <- length(bern_samplemeans)-1
  
  # get maximums for y limits of plot
  if(class(dd_plotdata())=="matrix"){
    maxfreq <- max(colSums(dd_plotdata()))
    maxdeg_obs <- dim(dd_plotdata())[2]-1
  } else {
    maxfreq <- max(dd_plotdata())
    maxdeg_obs <- length(dd_plotdata())-1
  }
  maxfreq_samples <- max(max(bern_upperline), max(unif_upperline))
  ylimit <- max(maxfreq, maxfreq_samples)
  
  if(input$densplotgroup == "percent"){
    plotme <- dd_plotdata()/sum(dd_plotdata())
    unif_samplemeans <- unif_samplemeans/sum(dd_plotdata())
    unif_upperline <- unif_upperline/sum(dd_plotdata())
    unif_lowerline <- unif_lowerline/sum(dd_plotdata())
    bern_samplemeans <- bern_samplemeans/sum(dd_plotdata())
    bern_upperline <- bern_upperline/sum(dd_plotdata())
    bern_lowerline <- bern_lowerline/sum(dd_plotdata())
    ylimit <- max(maxfreq/sum(dd_plotdata()), max(unif_upperline), max(bern_upperline))
    ylabel <- 'Percent of Nodes'
  }
  
  # make sure that barplot and lines have the same length
  maxdeg_total <- max(maxdeg_obs, maxdeg_u, maxdeg_b)
  if(maxdeg_u < maxdeg_total){
    unif_samplemeans <- append(unif_samplemeans, rep(0, times=maxdeg_total-maxdeg_u))
    unif_upperline <- append(unif_upperline, rep(0, times=maxdeg_total-maxdeg_u))
    unif_lowerline <- append(unif_lowerline, rep(0, times=maxdeg_total-maxdeg_u))
  } 
  if(maxdeg_b < maxdeg_total){
    bern_samplemeans <- append(bern_samplemeans, rep(0, times=maxdeg_total-maxdeg_b))
    bern_upperline <- append(bern_upperline, rep(0, times=maxdeg_total-maxdeg_b))
    bern_lowerline <- append(bern_lowerline, rep(0, times=maxdeg_total-maxdeg_b))
  }
  if(maxdeg_obs < maxdeg_total){
    if(class(plotme)=="matrix"){
      nrows <- dim(plotme)[1]
      plotme <- cbind(plotme, matrix(0, nrow=nrows, ncol= maxdeg_total-maxdeg_obs))
      colnames(plotme) <- paste(0:maxdeg_total)
    } else {
      plotme <- append(plotme, rep(0,times=maxdeg_total-maxdeg_obs))
      names(plotme) <- paste(0:maxdeg_total)
    }
  }
  
  #save x-coordinates of bars, so that points are centered on bars
  bar_axis <- barplot(plotme, xlab="Degree", ylab=ylabel,
                      col=color, ylim=c(0,ylimit), plot=TRUE)
  if(input$uniformoverlay_dd){
    points(x=bar_axis-.15, y=unif_samplemeans,col='firebrick', lwd=1, pch=18, cex=1.25)
    arrows(x0=bar_axis-.15, y0=unif_upperline, x1=bar_axis-.15, y1=unif_lowerline,
           code=3, length=0.1, angle=90, col='firebrick')
    ltext <- append(ltext, "CUG")
    lcol <- append(lcol, "firebrick")
    lty <- append(lty, 1)
    lpch <- append(lpch, 18)
    lfill <- append(lfill, 0)
    lborder <- append(lborder, 0)
  }
  if(input$bernoullioverlay_dd){
    points(x=bar_axis+.15, y=bern_samplemeans,col='orangered', lwd=1, pch=18, cex=1.25)
    arrows(x0=bar_axis+.15, y0=bern_upperline, x1=bar_axis+.15, y1=bern_lowerline,
           code=3, length=0.1, angle=90, col='orangered')
    ltext <- append(ltext, "BRG")
    lcol <- append(lcol, "orangered")
    lty <- append(lty, 1)
    lpch <- append(lpch, 18)
    lfill <- append(lfill, 0)
    lborder <- append(lborder, 0)
  }
  if(input$colorby_dd != "None" | input$uniformoverlay_dd | input$bernoullioverlay_dd){
    if(input$uniformoverlay_dd | input$bernoullioverlay_dd){
      lmerge <- TRUE
    } else {
      lmerge <-FALSE
      lpch <-NULL
    }
    legend(x="topright", legend=ltext, title=ltitle, fill=lfill, border=lborder,
           col=lcol, lty= lty, pch=lpch, pt.cex=1.25, bty="n", merge=lmerge)
  }
})

output$degreedistdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_degreedist.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=10)
    plotme <- dd_plotdata()
    color <- "#79AED4"
    ylabel <- "Count of Nodes"
    ltext <- c()
    lcol <- c() #color for lines
    lty <- c()
    lpch <- c()
    lfill <- c() #color for boxes
    lborder <- c()
    ltitle <- NULL
    if(!is.null(input$colorby_dd)){
      if(input$colorby_dd != "None"){
        ncolors <- dim(dd_plotdata())[1]
        color <- brewer.pal(ncolors,"Blues")[1:ncolors]
        ltext <- sort(unique(get.vertex.attribute(network(),input$colorby_dd)))
        ltext <- append(ltext, "")
        lfill <- c(color, 0)
        lborder <- append(lborder, c(rep("black", times=ncolors), 0))
        lty <- rep(0, times=ncolors+1)
        lpch <- rep(26, times=ncolors+1)
        ltitle <- input$colorby_dd
      }}
    
    unif_samplemeans <- dd_uniformoverlay()[[1]]
    unif_stderr <- dd_uniformoverlay()[[2]]
    unif_upperline <- unif_samplemeans + 2*unif_stderr
    unif_lowerline <- unif_samplemeans - 2*unif_stderr
    maxdeg_u <- length(unif_samplemeans)-1
    
    bern_samplemeans <- dd_bernoullioverlay()[[1]]
    bern_stderr <- dd_bernoullioverlay()[[2]]
    bern_upperline <- bern_samplemeans + 2*bern_stderr
    bern_lowerline <- bern_samplemeans - 2*bern_stderr
    maxdeg_b <- length(bern_samplemeans)-1
    
    # get maximums for y limits of plot
    if(class(dd_plotdata())=="matrix"){
      maxfreq <- max(colSums(dd_plotdata()))
      maxdeg_obs <- dim(dd_plotdata())[2]-1
    } else {
      maxfreq <- max(dd_plotdata())
      maxdeg_obs <- length(dd_plotdata())-1
    }
    maxfreq_samples <- max(max(bern_upperline), max(unif_upperline))
    ylimit <- max(maxfreq, maxfreq_samples)
    
    if(input$densplot == "Percent of nodes"){
      plotme <- dd_plotdata()/sum(dd_plotdata())
      unif_samplemeans <- unif_samplemeans/sum(dd_plotdata())
      unif_upperline <- unif_upperline/sum(dd_plotdata())
      unif_lowerline <- unif_lowerline/sum(dd_plotdata())
      bern_samplemeans <- bern_samplemeans/sum(dd_plotdata())
      bern_upperline <- bern_upperline/sum(dd_plotdata())
      bern_lowerline <- bern_lowerline/sum(dd_plotdata())
      ylimit <- max(maxfreq/sum(dd_plotdata()), max(unif_upperline), max(bern_upperline))
      ylabel <- 'Percent of Nodes'
    }
    
    # make sure that barplot and lines have the same length
    maxdeg_total <- max(maxdeg_obs, maxdeg_u, maxdeg_b)
    if(maxdeg_u < maxdeg_total){
      unif_samplemeans <- append(unif_samplemeans, rep(0, times=maxdeg_total-maxdeg_u))
      unif_upperline <- append(unif_upperline, rep(0, times=maxdeg_total-maxdeg_u))
      unif_lowerline <- append(unif_lowerline, rep(0, times=maxdeg_total-maxdeg_u))
    } 
    if(maxdeg_b < maxdeg_total){
      bern_samplemeans <- append(bern_samplemeans, rep(0, times=maxdeg_total-maxdeg_b))
      bern_upperline <- append(bern_upperline, rep(0, times=maxdeg_total-maxdeg_b))
      bern_lowerline <- append(bern_lowerline, rep(0, times=maxdeg_total-maxdeg_b))
    }
    if(maxdeg_obs < maxdeg_total){
      if(class(plotme)=="matrix"){
        nrows <- dim(plotme)[1]
        plotme <- cbind(plotme, matrix(0, nrow=nrows, ncol= maxdeg_total-maxdeg_obs))
        colnames(plotme) <- paste(0:maxdeg_total)
      } else {
        plotme <- append(plotme, rep(0,times=maxdeg_total-maxdeg_obs))
        names(plotme) <- paste(0:maxdeg_total)
      }
    }
    
    #save x-coordinates of bars, so that points are centered on bars
    bar_axis <- barplot(plotme, xlab="Degree", ylab=ylabel,
                        col=color, ylim=c(0,ylimit), plot=TRUE)
    if(input$uniformoverlay_dd){
      points(x=bar_axis-.15, y=unif_samplemeans,col='firebrick', lwd=1, pch=18, cex=1.25)
      arrows(x0=bar_axis-.15, y0=unif_upperline, x1=bar_axis-.15, y1=unif_lowerline,
             code=3, length=0.1, angle=90, col='firebrick')
      ltext <- append(ltext, "CUG")
      lcol <- append(lcol, "firebrick")
      lty <- append(lty, 1)
      lpch <- append(lpch, 18)
      lfill <- append(lfill, 0)
      lborder <- append(lborder, 0)
    }
    if(input$bernoullioverlay_dd){
      points(x=bar_axis+.15, y=bern_samplemeans,col='orangered', lwd=1, pch=18, cex=1.25)
      arrows(x0=bar_axis+.15, y0=bern_upperline, x1=bar_axis+.15, y1=bern_lowerline,
             code=3, length=0.1, angle=90, col='orangered')
      ltext <- append(ltext, "BRG")
      lcol <- append(lcol, "orangered")
      lty <- append(lty, 1)
      lpch <- append(lpch, 18)
      lfill <- append(lfill, 0)
      lborder <- append(lborder, 0)
    }
    if(input$colorby_dd != "None" | input$uniformoverlay_dd | input$bernoullioverlay_dd){
      if(input$uniformoverlay_dd | input$bernoullioverlay_dd){
        lmerge <- TRUE
      } else {
        lmerge <-FALSE
        lpch <-NULL
      }
      legend(x="topright", legend=ltext, title=ltitle, fill=lfill, border=lborder,
             col=lcol, lty= lty, pch=lpch, pt.cex=1.25, bty="n", merge=lmerge)
    }
    dev.off()
})

#GEODESIC DISTRIBUTION

gd_uniformoverlay <- reactive({
  if(!is.network(network())){
    return()
  }
  gd <- geodist(uniformsamples(), count.paths=FALSE, inf.replace=NA)
  maxgeo <- max(unlist(gd), na.rm=TRUE)
  reps <- length(gd)
  for(k in 1:reps){
    gd[[k]]$gdist[is.na(gd[[k]]$gdist)] <- Inf
  }
  gd_data <- lapply(gd,function(x){tabulate(x$gdist, nbins=maxgeo)})
    #list of tabulated geodesics for each draw, except those of 0 or Inf length
  gd_data_complete <- matrix(0, nrow=maxgeo+1, ncol=reps)
  for(k in 1:reps){
    gd_data[[k]] <- append(gd_data[[k]], sum(gd[[k]]$gdist == Inf))
    gd_data_complete[,k] <- gd_data[[k]]
  }
  
  geomeans <- apply(gd_data_complete, MARGIN=1, FUN=mean)
  names(geomeans) <- c(paste(1:maxgeo), "Inf")
  geosd <- apply(gd_data_complete, MARGIN=1, FUN=sd)
  
  mean_and_sd <- list(geomeans, geosd)
})

gd_bernoullioverlay <- reactive({
  if(!is.network(network())){
    return()
  }
  gd <- geodist(bernoullisamples(), count.paths=FALSE, inf.replace=NA)
  maxgeo <- max(unlist(gd), na.rm=TRUE)
  reps <- length(gd)
  for(k in 1:reps){
    gd[[k]]$gdist[is.na(gd[[k]]$gdist)] <- Inf
  }
  gd_data <- lapply(gd,function(x){tabulate(x$gdist, nbins=maxgeo)})
  #list of tabulated geodesics for each draw, except those of 0 or Inf length
  gd_data_complete <- matrix(0, nrow=maxgeo+1, ncol=reps)
  for(k in 1:reps){
    gd_data[[k]] <- append(gd_data[[k]], sum(gd[[k]]$gdist == Inf))
    gd_data_complete[,k] <- gd_data[[k]]
  }
  
  geomeans <- apply(gd_data_complete, MARGIN=1, FUN=mean)
  names(geomeans) <- c(paste(1:maxgeo), "Inf")
  geosd <- apply(gd_data_complete, MARGIN=1, FUN=sd)
  
  mean_and_sd <- list(geomeans, geosd)
})

output$geodistplot <- renderPlot({
  if(!is.network(network())){
    return()
  }
  g <- geodist(network(),inf.replace=NA)
  gdata <- tabulate(g$gdist)
  g$gdist[is.na(g$gdist)] <- Inf
  gdata <- append(gdata, sum(g$gdist == Inf))
  maxgeo <- length(gdata)-1
  names(gdata) <- c(paste(1:maxgeo), "Inf")
  
  unif_means <- gd_uniformoverlay()[[1]]
  unif_stderr <- gd_uniformoverlay()[[2]]
  unif_upperline <- unif_means + 2*unif_stderr
  unif_lowerline <- unif_means - 2*unif_stderr
  maxgeo_u <- length(unif_means)-1
  
  bern_means <- gd_bernoullioverlay()[[1]]
  bern_stderr <- gd_bernoullioverlay()[[2]]
  bern_upperline <- bern_means + 2*bern_stderr
  bern_lowerline <- bern_means - 2*bern_stderr
  maxgeo_b <- length(bern_means)-1

  ylabel <- "Count of Vertex Pairs"
  
  #for density plot
  if(input$densplotgroup_gd == "percent"){
    unif_means <- unif_means/sum(gdata)
    unif_upperline <- unif_upperline/sum(gdata)
    unif_lowerline <- unif_lowerline/sum(gdata)
    bern_means <- bern_means/sum(gdata)
    bern_upperline <- bern_upperline/sum(gdata)
    bern_lowerline <- bern_lowerline/sum(gdata)
    gdata <- gdata/sum(gdata)
    ylimit <- max(gdata, bern_upperline, unif_upperline)
    ylabel <- "Percent of Vertex Pairs"
  }
  #get maximums to set y limits
  ylimit <- max(gdata, bern_upperline, unif_upperline)
  
  # make sure that barplot and lines have the same length
  maxgeo_total <- max(maxgeo, maxgeo_u, maxgeo_b)
  if(maxgeo_u < maxgeo_total){
    unif_means <- append(unif_means, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_means)-1)
    unif_upperline <- append(unif_upperline, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_upperline)-1)
    unif_lowerline <- append(unif_lowerline, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_lowerline)-1)
  } 
  if(maxgeo_b < maxgeo_total){
    bern_means <- append(bern_means, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_means)-1)
    bern_upperline <- append(bern_upperline, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_upperline)-1)
    bern_lowerline <- append(bern_lowerline, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_lowerline)-1)
  }
  if(maxgeo < maxgeo_total){
    gdata <- append(gdata, rep(0,times=maxgeo_total-maxgeo), after=length(gdata)-1)
    names(gdata) <- c(paste(1:maxgeo_total), "Inf")
    
  }
  
  ltext <- c()
  lcol <- c()
  
  #save x-coordinates of bars, so that points are centered on bars
  bar_axis <- barplot(gdata,  col="#79AED4",
                      xlab = "Geodesic Value", ylab = ylabel,
                      ylim = c(0,ylimit), plot=TRUE)
  
  if(input$uniformoverlay_gd){
    points(x=bar_axis-.15, y=unif_means,col='firebrick', lwd=1, pch=18, cex=1.25)
    arrows(x0=bar_axis-.15, y0=unif_upperline, x1=bar_axis-.15, y1=unif_lowerline,
           code=3, length=0.1, angle=90, col='firebrick')
    ltext <- append(ltext, "CUG")
    lcol <- append(lcol, "firebrick")
  }
  if(input$bernoullioverlay_gd){
    points(x=bar_axis+.15, y=bern_means,col='orangered', lwd=1, pch=18, cex=1.25)
    arrows(x0=bar_axis+.15, y0=bern_upperline, x1=bar_axis+.15, y1=bern_lowerline,
           code=3, length=0.1, angle=90, col='orangered')
    ltext <- append(ltext, "BRG")
    lcol <- append(lcol, "orangered")
  }
  if(input$uniformoverlay_gd | input$bernoullioverlay_gd){
    legend(x="topright", legend=ltext, col=lcol, lwd=1, pch=18, pt.cex=1.25, merge=TRUE,
           inset=c(.12,0), bty="n")
  }
  
})

output$geodistdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_geodist.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=15)
    g <- geodist(network(),inf.replace=NA)
    gdata <- tabulate(g$gdist)
    g$gdist[is.na(g$gdist)] <- Inf
    gdata <- append(gdata, sum(g$gdist == Inf))
    maxgeo <- length(gdata)-1
    names(gdata) <- c(paste(1:maxgeo), "Inf")
    
    unif_means <- gd_uniformoverlay()[[1]]
    unif_stderr <- gd_uniformoverlay()[[2]]
    unif_upperline <- unif_means + 2*unif_stderr
    unif_lowerline <- unif_means - 2*unif_stderr
    maxgeo_u <- length(unif_means)-1
    
    bern_means <- gd_bernoullioverlay()[[1]]
    bern_stderr <- gd_bernoullioverlay()[[2]]
    bern_upperline <- bern_means + 2*bern_stderr
    bern_lowerline <- bern_means - 2*bern_stderr
    maxgeo_b <- length(bern_means)-1
    
    ylabel <- "Count of Vertex Pairs"
    
    #for density plot
    if(input$densplot_gd == "Percent of vertex pairs"){
      unif_means <- unif_means/sum(gdata)
      unif_upperline <- unif_upperline/sum(gdata)
      unif_lowerline <- unif_lowerline/sum(gdata)
      bern_means <- bern_means/sum(gdata)
      bern_upperline <- bern_upperline/sum(gdata)
      bern_lowerline <- bern_lowerline/sum(gdata)
      gdata <- gdata/sum(gdata)
      ylimit <- max(gdata, bern_upperline, unif_upperline)
      ylabel <- "Percent of Vertex Pairs"
    }
    #get maximums to set y limits
    ylimit <- max(gdata, bern_upperline, unif_upperline)
    
    # make sure that barplot and lines have the same length
    maxgeo_total <- max(maxgeo, maxgeo_u, maxgeo_b)
    if(maxgeo_u < maxgeo_total){
      unif_means <- append(unif_means, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_means)-1)
      unif_upperline <- append(unif_upperline, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_upperline)-1)
      unif_lowerline <- append(unif_lowerline, rep(0, times=maxgeo_total-maxgeo_u), after=length(unif_lowerline)-1)
    } 
    if(maxgeo_b < maxgeo_total){
      bern_means <- append(bern_means, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_means)-1)
      bern_upperline <- append(bern_upperline, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_upperline)-1)
      bern_lowerline <- append(bern_lowerline, rep(0, times=maxgeo_total-maxgeo_b), after=length(bern_lowerline)-1)
    }
    if(maxgeo < maxgeo_total){
      gdata <- append(gdata, rep(0,times=maxgeo_total-maxgeo), after=length(gdata)-1)
      names(gdata) <- c(paste(1:maxgeo_total), "INF")
      
    }
    
    ltext <- c()
    lcol <- c()
    
    #save x-coordinates of bars, so that points are centered on bars
    bar_axis <- barplot(gdata,  col="#79AED4",
                        xlab = "Geodesic Value", ylab = ylabel,
                        ylim = c(0,ylimit), plot=TRUE)
    
    if(input$uniformoverlay_gd){
      points(x=bar_axis-.15, y=unif_means,col='firebrick', lwd=1, pch=18, cex=1.25)
      arrows(x0=bar_axis-.15, y0=unif_upperline, x1=bar_axis-.15, y1=unif_lowerline,
             code=3, length=0.1, angle=90, col='firebrick')
      ltext <- append(ltext, "CUG")
      lcol <- append(lcol, "firebrick")
    }
    if(input$bernoullioverlay_gd){
      points(x=bar_axis+.15, y=bern_means,col='orangered', lwd=1, pch=18, cex=1.25)
      arrows(x0=bar_axis+.15, y0=bern_upperline, x1=bar_axis+.15, y1=bern_lowerline,
             code=3, length=0.1, angle=90, col='orangered')
      ltext <- append(ltext, "BRG")
      lcol <- append(lcol, "orangered")
    }
    if(input$uniformoverlay_gd | input$bernoullioverlay_gd){
      legend(x="topright", legend=ltext, col=lcol, lwd=1, pch=18, pt.cex=1.25, merge=TRUE,
             inset=c(.12,0), bty="n")
    }
    dev.off()
  })

#MORE

observe({
  if(input$plottabs == "More"){
    updateTabsetPanel(session, 'displaytabs', selected="Network Summary")
  }
})

# FUTURE: will be able to subset data
# output$subsetting <- renderUI({
#   if(class(network())!='network'){
#     return()
#   }
#   selectInput('subsetattr', label=NULL,
#               choices = c('None', menuattr()), selectize = FALSE)
# })
# outputOptions(output,'subsetting',suspendWhenHidden=FALSE)
# 
# output$subsetting2 <- renderUI({
#   if(class(network())!='network' | input$subsetattr == "None"){
#     return()
#   }
#   choices <- sort(unique(get.vertex.attribute(network(),input$subsetattr)))
#   checkboxGroupInput('subsetattrchoice', label=NULL,
#                      choices=choices, selected=NULL)
# })

#since the visibility toggles between two states, set the options to 
#not suspend the output when hidden
output$mixmxchooser <- renderUI({
  selectInput('mixmx', label='Choose attribute',
              choices = menuattr(), selectize = FALSE)
})
outputOptions(output,'mixmxchooser',suspendWhenHidden=FALSE)

output$mixingmatrix <- renderPrint({
  if(!is.network(network())) {return()}
  if(!is.null(input$mixmx)){
  mixingmatrix(network(), input$mixmx)}
})
outputOptions(output,'mixingmatrix',suspendWhenHidden=FALSE)

output$gden <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  gden(network(), diag=has.loops(network()), mode=gmode)
})
outputOptions(output,'gden',suspendWhenHidden=FALSE)

output$grecip <- renderText({
  if(!is.network(network())) {return()}
  if(input$grecipmeas == ''){
    return('Reciprocity:')
  }
  grecip(network(), measure=input$grecipmeas)
})
outputOptions(output,'grecip',suspendWhenHidden=FALSE)

output$gtrans <- renderText({
  if(!is.network(network())) {return()}
  if(input$gtransmeas == ''){
    return('Transitivity:')
  }
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  gtrans(network(), diag=has.loops(network()), mode=gmode,
         measure=input$gtransmeas)
})
outputOptions(output,'gtrans',suspendWhenHidden=FALSE)

output$gdeg <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- centralization(network(), degree, mode=gmode, diag=has.loops(network()),
              cmode=input$gdegcmode)
})
outputOptions(output,'gdeg',suspendWhenHidden=FALSE)

output$gbetw <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- centralization(network(), betweenness, mode=gmode, diag=has.loops(network()),
                   cmode=input$gbetwcmode)
})
outputOptions(output,'gbetw',suspendWhenHidden=FALSE)

output$gclose <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- centralization(network(), closeness, mode=gmode, diag=has.loops(network()),
                 cmode=input$gclosecmode)
})
outputOptions(output,'gclose',suspendWhenHidden=FALSE)

output$gstress <- renderText({
  if(!is.network(network())){ return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- centralization(network(), stresscent, mode=gmode, diag=has.loops(network()),
                  cmode=input$gstresscmode)
})
outputOptions(output,'gstress',suspendWhenHidden=FALSE)

output$ggraphcent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- centralization(network(), graphcent, mode=gmode, diag=has.loops(network()),
                 cmode=input$ggraphcentcmode)
})
outputOptions(output,'ggraphcent',suspendWhenHidden=FALSE)

output$gevcent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- centralization(network(), evcent, mode=gmode, diag=has.loops(network()))
})
outputOptions(output,'gevcent',suspendWhenHidden=FALSE)

output$ginfocent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-''
  try({
    i <- centralization(network(), infocent, mode=gmode, diag=has.loops(network()),
                  cmode=input$ginfocentcmode)})
  i
})
outputOptions(output,'ginfocent',suspendWhenHidden=FALSE)

output$ndeg <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
         cmode=input$ndegcmode)
})
outputOptions(output,'ndeg',suspendWhenHidden=FALSE)

output$ndegmin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(network(), gmode=gmode, diag=has.loops(network()),
              cmode=input$ndegcmode)
  min(d)
})
outputOptions(output,'ndegmin',suspendWhenHidden=FALSE)

output$ndegmax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(network(), gmode=gmode, diag=has.loops(network()),
              cmode=input$ndegcmode)
  max(d)
})
outputOptions(output,'ndegmax',suspendWhenHidden=FALSE)

output$nbetw <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
                   cmode=input$nbetwcmode)
})
outputOptions(output,'nbetw',suspendWhenHidden=FALSE)

output$nbetwmin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(network(), gmode=gmode, diag=has.loops(network()),
                   cmode=input$nbetwcmode)
  min(b)
})
outputOptions(output,'nbetwmin',suspendWhenHidden=FALSE)

output$nbetwmax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(network(), gmode=gmode, diag=has.loops(network()),
                   cmode=input$nbetwcmode)
  max(b)
})
outputOptions(output,'nbetwmax',suspendWhenHidden=FALSE)

output$nclose <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
                 cmode=input$nclosecmode)
})
outputOptions(output,'nclose',suspendWhenHidden=FALSE)

output$nclosemin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(network(), gmode=gmode, diag=has.loops(network()),
                 cmode=input$nclosecmode)
  min(c)
})
outputOptions(output,'nclosemin',suspendWhenHidden=FALSE)

output$nclosemax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(network(), gmode=gmode, diag=has.loops(network()),
                 cmode=input$nclosecmode)
  max(c)
})
outputOptions(output,'nclosemax',suspendWhenHidden=FALSE)

output$nstress <- renderText({
  if(!is.network(network())){ return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
                  cmode=input$nstresscmode)
})
outputOptions(output,'nstress',suspendWhenHidden=FALSE)

output$nstressmin <- renderText({
  if(!is.network(network())){ return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(network(), gmode=gmode, diag=has.loops(network()),
                  cmode=input$nstresscmode)
  min(s)
})
outputOptions(output,'nstressmin',suspendWhenHidden=FALSE)

output$nstressmax <- renderText({
  if(!is.network(network())){ return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(network(), gmode=gmode, diag=has.loops(network()),
                  cmode=input$nstresscmode)
  max(s)
})
outputOptions(output,'nstressmax',suspendWhenHidden=FALSE)

output$ngraphcent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
                  cmode=input$ngraphcentcmode)
})
outputOptions(output,'ngraphcent',suspendWhenHidden=FALSE)

output$ngraphcentmin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(network(), gmode=gmode, diag=has.loops(network()),
                 cmode=input$ngraphcentcmode)
  min(g)
})
outputOptions(output,'ngraphcentmin',suspendWhenHidden=FALSE)

output$ngraphcentmax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(network(), gmode=gmode, diag=has.loops(network()),
                 cmode=input$ngraphcentcmode)
  max(g)
})
outputOptions(output,'ngraphcentmax',suspendWhenHidden=FALSE)

output$nevcent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()))
})
outputOptions(output,'nevcent',suspendWhenHidden=FALSE)

output$nevcentmin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(network(), gmode=gmode, diag=has.loops(network()))
  min(e)
})
outputOptions(output,'nevcentmin',suspendWhenHidden=FALSE)

output$nevcentmax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(network(), gmode=gmode, diag=has.loops(network()))
  max(e)
})
outputOptions(output,'nevcentmax',suspendWhenHidden=FALSE)

output$ninfocent <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-''
  try({
    i <- infocent(network(), nodes=input$nodeind, gmode=gmode, diag=has.loops(network()),
                   cmode=input$ninfocentcmode)})
  i
})
outputOptions(output,'ninfocent',suspendWhenHidden=FALSE)

output$ninfocentmin <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-''
  try({
    i <- infocent(network(), gmode=gmode, diag=has.loops(network()),
                  cmode=input$ninfocentcmode)
    i<-min(i)})
  i
})
outputOptions(output,'ninfocentmin',suspendWhenHidden=FALSE)

output$ninfocentmax <- renderText({
  if(!is.network(network())) {return()}
  if(is.directed(network())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i <- ''
  try({
    i <- infocent(network(), gmode=gmode, diag=has.loops(network()),
                  cmode=input$ninfocentcmode)
    i<-max(i)})
  i
})
outputOptions(output,'ninfocentmax',suspendWhenHidden=FALSE)

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
# UNCOMMENT AFTER RELEASE FOR TERM DOCUMENTATION
# output$listofterms <- renderUI({
#   if(!is.network(network())){
#     return()
#   }
#   if(input$matchingorall == 'All terms'){
#     current.terms <- unlist(allterms)
#   } else {
#     matchterms <- search.ergmTerms(net=network())
#     ind <- gregexpr(pattern='\\(', matchterms)
#     for(i in 1:length(matchterms)){
#       matchterms[i] <- substr(matchterms[[i]], start=1, stop=ind[[i]][1]-1)
#     }
#     matchterms <- unique(matchterms)
#     current.terms <- unlist(matchterms)
#   }
#   selectInput('termdoc',label = NULL,
#                   choices = current.terms,
#                   multiple=FALSE, 
#                   selectize=FALSE)
#   
# })

# output$termdoc <- renderPrint({
#   myterm <- input$termdoc
#   search.ergmTerms(name=myterm)
# })


#' Below I output the current formulation of the ergm 
#' model so the user can clearly see how their menu selections change the model.
#' Since `ergm.terms()` is a reactive object, it will automatically update when
#' the user clicks on menu options.
#'  
#+ fitmodel2, eval=FALSE
output$currentdataset1 <- renderPrint({
  if(!is.network(network())){
    return(cat('Upload a network'))
  }
  cat(isolate(nwname()))
})

output$checkterms1 <- renderPrint({
  if(!is.network(network())){
    return(cat('Upload a network'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
  cat(ergm.terms())
})

output$prefitsum <- renderPrint({
  if(!is.network(network()) | length(input$terms)==0){
    return(cat('NA'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
  options(width=150)
  summary(ergm.formula())
})

output$modelfit <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  model1reac()
})

output$modelfitsum <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  summary(model1reac())
})

#make sure that mcmc iterations output on the fitting tab by allowing
#modelfit to update even when other tab is active

#other potential methods were to use onFlush, or to set the priority of
#observers, but this is the best and least complicated (and the only 
#one that works)

outputOptions(output, "modelfit",priority=10, suspendWhenHidden=FALSE)
outputOptions(output, "modelfitsum",priority=-10)

#' **Diagnostics - MCMC Diagnostics**
#' 
#' When using the `mcmc.diagnostics` function in the command line, the printed 
#' diagnostics and plots all output together. Instead of calling `mcmc.diagnositcs`
#' a reactive object, .
#' 
#+ eval=FALSE

output$checkterms3 <- renderPrint({
  if(is.null(network())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  cat(isolate(ergm.terms()))
})
output$currentdataset3 <- renderPrint({
  if(!is.network(network())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

output$diagnosticsplot <- renderPlot({
  vpp <- length(model1reac()$coef)
  tryCatch(
    mcmc.diagnostics(model1reac(), vars.per.page = vpp),
    error = function(e) cat("MCMC was not run or MCMC sample was not stored"))
})

output$mcmcplotdownload <- downloadHandler(
  vpp <- length(model1reac()$coef),
  filename = function(){paste(nwname(),'_mcmc.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=vpp*4/3, width=10)
    tryCatch(
      mcmc.diagnostics(model1reac(), vars.per.page = vpp),
      error = function(e) cat("MCMC was not run or MCMC sample was not stored"))
    dev.off()
  }
)

output$diagnosticsplotspace <- renderUI({
  if(input$fitButton == 0){
    return()
  }
  vpp <- length(model1reac()$coef) 
  plotOutput('diagnosticsplot', height = vpp*400/2)
})

output$diagnostics <- renderPrint({
  if(input$fitButton == 0){
    return()
  }
  isolate(tryCatch(
    mcmc.diagnostics(model1reac()),
    error = function(e) cat("MCMC was not run or MCMC sample was not stored")))
})
outputOptions(output, 'diagnostics', suspendWhenHidden=FALSE)



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
  if(!is.network(network())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

#formula only updates after fitButton has been clicked
output$checkterms2 <- renderPrint({
  if(!is.network(network())){
    return(cat('Upload a network'))
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
  input$gofButton
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
  if(!is.network(network())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  cat(isolate(ergm.terms()))
})
output$currentdataset4 <- renderPrint({
  if(!is.network(network())){
    return(cat('Upload a network'))
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
outputOptions(output,'dynamiccolor2',suspendWhenHidden=FALSE)

output$colorwarning2 <- renderUI({
  if(length(legendlabels2())<10) return()
  span(tags$u('Warning:'), ' Colors get recycled for attributes with',
       'more than nine levels.', style='font-size:0.85em;')
})

output$dynamicsize2 <- renderUI({
  selectInput('sizeby2',
              label = 'Size nodes according to:',
              c('None' = 1, 'Betweenness',numattr()),
              selectize = FALSE)
})


output$simplot <- renderPlot({
  if(input$simButton == 0){
    return()
  }
  nw <- network()
  nsims <- isolate(input$nsims)
  model1sim <- isolate(model1simreac()) 
  
  #can't plot simulation number greater than total sims
  if(input$thissim > nsims){
    return()
  } 
  
  color <- adjustcolor(vcol2(), alpha.f = input$transp2)
  
  if (nsims == 1){
    plot(model1sim, coord = sim.coords.1(), 
         displayisolates = input$iso2, 
         displaylabels = input$vnames2, 
         vertex.col = color,
         vertex.cex = nodesize2())
  } else {
    plot(model1sim[[input$thissim]], 
         coord = sim.coords.2(),
         displayisolates = input$iso2, 
         displaylabels = input$vnames2, 
         vertex.col = color,
         vertex.cex = nodesize2())
  }
  if(input$colorby2 != 2){
      legend('bottomright', title=input$colorby2, legend = legendlabels2(), fill = legendfill2())
    }
  
})

output$simplotdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_simplot.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=10)
    color <- adjustcolor(vcol2(), alpha.f = input$transp2)
    if(input$nsims == 1){
    plot(model1simreac(), 
         coord = sim.coords.1(), 
         displayisolates = input$iso2, 
         displaylabels = input$vnames2, 
         vertex.col = color,
         vertex.cex = nodesize2())
    }else{
      plot(model1simreac()[[input$thissim]], 
           coord = sim.coords.2(), 
           displayisolates = input$iso2, 
           displaylabels = input$vnames2, 
           vertex.col = color,
           vertex.cex = nodesize2())
    }
    if(input$colorby2 != 2){
      legend('bottomright', title=input$colorby2, legend = legendlabels2(), fill = legendfill2())
    }
    dev.off()
  }
)


})

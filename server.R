#' ---
#' title: "statnetWeb, server.R"
#' author: "Emily Beylerian"
#' ---
#' statnetWeb
#' ===========
#' server.R
#' ===========

#' **Before reading this document:** The Shiny app "statnetWeb" is not contained in a
#' single R Script. Within the folder "statnetWeb" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files. If you are
#' unfamiliar with Shiny apps, it may be more natural and helpful to start with the 
#' documentation for `ui.R` and then move on to `server.R`.
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
library(shinyAce)
library(ergm)
library(sna)
library(network)
library(RColorBrewer)
library(lattice)
library(latticeExtra)
#source("chooser.R")
source("modelcomp.R")

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

shinyServer(
  function(input, output, session){

    
#' To keep a list of all attributes uploaded by the user:  
#' can't use global variables because they are common to all 
#' sessions using the app and will get overwritten, instead 
#' created per session variables inside shinyServer
#' 
#' 
#+ eval=FALSE
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
assign('vertex_names', list(),
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
#' definition of the numeric vertex attributes, we call `attrib()`.    
#+ eval=FALSE

values <- reactiveValues()

#move to Data panel when user clicks Get Started button
observe({
  if(input$startButton == 0) {return()}
  isolate({
    updateTabsetPanel(session, 'navbar', selected='tab2')
  })
})

#update active tab in navbar when arrows are clicked
leftarrowclicks <- reactive({
  input$dataleft+input$plotleft+input$fitleft+input$mcmcleft+input$gofleft+input$simleft
})
rightarrowclicks <- reactive({
  input$dataright+input$plotright+input$fitright+input$mcmcright+input$gofright+input$simright
})
observe({
  if(leftarrowclicks() == 0) {return()}
  tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6', 'tab7', 'tab8')
  current <- isolate(which(input$navbar==tabOptions))
  updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
})
observe({
  if(rightarrowclicks() == 0) {return()}
  tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6', 'tab7', 'tab8')
  current <- isolate(which(input$navbar==tabOptions))
  updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
})

acecode <- reactiveValues(data="library(statnet)")

#this reactive expression is used to get the initial values of the network
nwinit <- reactive({
  #input$rawdatafile comes as a dataframe with name, size, type and datapath
  #datapath is stored in 4th column of dataframe
  #network creates a network object from the input file
  if(is.null(input$rawdatafile)){
    nw_var <- NULL
  } else {
    filepath <- input$rawdatafile[1,4]
    filename <- input$rawdatafile[1,1]
  }
  loadfile <- ''
  if(input$filetype == 1){
    loadfile <- ''
    if(!is.null(input$rawdatafile)){
      nw_var <- tryCatch({
        obj <- load(paste(filepath))
      }, error = function(err){
        return("Chosen file is not an R object")
      }, finally = NULL
      )
      try(nw_var <- get(obj))
      loadfile <- paste('load("',filepath,'")', '\n',
                        'nw <- ',obj, sep='')
    }
  } else if(input$filetype == 2){
    loadfile <- ''
    if(!is.null(input$rawdatafile)){
      nw_var <- "Upload a .net file"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".net" |
           substr(filename,nchar(filename)-3,nchar(filename))==".NET"){
        nw_var <- read.paj(paste(filepath))
        loadfile <- paste('nw <- read.paj("',filepath,'")', sep='')
      }
    }
  } else if(input$filetype == 3){
    loadfile <- ''
    if(!is.null(input$rawdatafile)){
      nw_var <- "Upload a .paj file"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".paj" |
           substr(filename,nchar(filename)-3,nchar(filename))==".PAJ"){
        nws <- read.paj(paste(filepath))
        if(!is.null(pajnws())){
          nw_var <- nws$networks[[as.numeric(input$choosepajnw)]]
          loadfile <- paste('nws <- read.paj("',filepath,'") \n',
                            'nw <- nws$networks[[',as.numeric(input$choosepajnw),']]', sep='')
        }
      }
    }
  } else if(input$filetype == 4){
    loadfile <- ''
    if(!is.null(input$rawdatafile)){
      nw_var <- "Input the specified type of matrix"
      if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){
        header <- TRUE
        row_names<-1
        if(input$matrixtype == "edgelist"){
          header <- FALSE
          row_names<-NULL
        }
        try({nw_var <- network(read.csv(paste(filepath), sep=",", header=header, row.names=row_names),
                          directed=input$dir, loops=input$loops,
                          multiple=input$multiple, bipartite=input$bipartite,
                          matrix.type=input$matrixtype,
                          ignore.eval=FALSE, names.eval='edgevalue')
             codeline1 <- paste('x <- read.csv("',filepath,'", sep=",", header=',header,
                           'row.names=',row_names,')', sep='')
             })
        
      }
      try({nw_var <- network(read.table(paste(filepath)),
                        directed=input$dir, loops=input$loops,
                        multiple=input$multiple, bipartite=input$bipartite,
                        matrix.type=input$matrixtype,
                        ignore.eval=FALSE, names.eval='edgevalue')
           codeline1 <- paste('x <- read.table("',filepath,'")',sep='')
           })
        
      
      codeline2 <- paste('nw <- network(x, directed=',input$dir,', loops=',input$loops,
                         ', multiple=',input$multiple,', bipartite=',input$bipartite,
                         ', matrix.type=',input$matrixtype,
                         ', ignore.eval=FALSE, names.eval="edgevalue")', sep='')
      loadfile <- paste(codeline1,"\n", codeline2, sep="")
    }
    
  } else if(input$filetype ==5){
    loadfile <- ''
    if(input$samplenet == "None"){
      nw_var <- NULL
    } else {
      nw_var <- eval(parse(text = input$samplenet))
      if(!is.element('bipartite',names(nw_var$gal))){
        set.network.attribute(nw_var,'bipartite',FALSE)
      }
      loadfile <- paste('data(',input$samplenet,') \n',
                        'nw <- ',input$samplenet, sep='')
    }
  }
  prev <- "library(statnet)"
  acecode$data <- paste(prev,"\n",loadfile, sep='')
  nw_var
})

observe({
  updateAceEditor(session, "dataAce", value=acecode$data)
})

#list of everything in an uploaded Pajek project
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

#set correct number of rows for the attribute value lists, 
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

#names of uploaded attributes
#or helpful message that upload is incorrect
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
      if(class(newattrs) != "list"){
        newname <- "Attribute is not compatible, see help buttons and try again"
      }
    }
  })
  if(is.null(newname)){
    newname <- "Attribute is not named,  please fix and re-upload"
  }
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
    nw_var <- nwinit()
  
    if (class(nw_var)=="network"){
      #preserve initial network attributes and let user choose if directed 
      #after symmetrizing
      if(input$symmetrize != "Do not symmetrize"){
        symnw <- symmetrize(nw_var, rule=input$symmetrize)
        if(input$aftersymm == 'directed'){
          nw_var <- network(symnw, matrix.type="adjacency", directed=TRUE,
                        hyper=nwattrinit()[2], loops=nwattrinit()[3],
                        multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
        } else {
          nw_var <- network(symnw, matrix.type="adjacency", directed=FALSE,
                        hyper=nwattrinit()[2], loops=nwattrinit()[3],
                        multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
        }
        #add initial vertex attributes back after symmetrizing
        #can't add edge attributes back because number of edges has changed
        for(k in 1:length(vattrinit())){
          attr_names <- vattrinit()
          attr_matrix <- vattrinit.vals()
          set.vertex.attribute(nw_var,attr_names[k],attr_matrix[,k])
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
        try(network.vertex.names(nw_var) <- vertex_names)
      })
      v_numnew <- length(v_attrNamesToAdd)
      if(v_numnew > 1){
        for (j in 2:v_numnew){
          try({v_newname <- as.character(v_attrNamesToAdd[1,j])
          v_newval <- v_attrValsToAdd[,j]
          set.vertex.attribute(nw_var,v_newname,v_newval)})
        }
      }
      
      e_numnew <- length(e_attrNamesToAdd)
      if(e_numnew > 1){
        for (k in 2:e_numnew){
          try({e_newname <- as.character(e_attrNamesToAdd[1,k])
          e_newval <- e_attrValsToAdd[,k]
          set.edge.attribute(nw_var,e_newname,e_newval)})
        }
      }
      
      ev_numnew <- length(ev_attrNamesToAdd)
      if(ev_numnew > 1){
        for (l in 2:ev_numnew){
          try({ev_newname <- as.character(ev_attrNamesToAdd[1,l])
          ev_newval <- ev_attrValsToAdd[[l]]
          set.edge.value(nw_var,ev_newname,ev_newval)})
        }
      }
    }
  
    nw_var
	})

#use this network for future calculations
nw <- reactive({
  nw_var <- nwmid()
  
#deleting attributes is no longer available

#   deleteme <- input$deleteattrs$right
#   len <- length(deleteme)
#   if(len>=1){
#       for(i in 1:len){
#         if(any(list.vertex.attributes(nw_var)==deleteme[i])){
#           delete.vertex.attribute(nw_var,deleteme[i])
#         }
#         if(any(list.edge.attributes(nw_var)==deleteme[i])){
#           delete.edge.attribute(nw_var,deleteme[i])
#         }
#       }
#   }
  assign('input_termslist', list(), pos='package:base')
  updateTextInput(session, inputId='terms', value='edges')

  nw_var
})


#get coordinates to plot network with
coords <- reactive({
		plot.network(nw())})

#initial network attributes
#returns vector of true/falses
nwattrinit <- reactive({
  if(!is.network(nwinit())){return()}
  nwattributes <- c('directed','hyper','loops','multiple','bipartite')
  unlist(lapply(nwattributes,get.network.attribute,x=nwinit()))
})

#list of all vertex attributes in nw (after adding new)
attrib <- reactive({
	  attr <- c()
    if(is.network(nw())){        
		    attr<-list.vertex.attributes(nw())
    }
      attr
  })

#don't allow "na" or "vertex.names" as vertex attributes in menus on fit tab
menuattr <- reactive({
  menuattr <- attrib()
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
    if(is.network(nw())){  
      for(i in 1:length(attrib())){
        if(is.numeric(get.vertex.attribute(nw(),attrib()[i]))){
          numattr <- append(numattr,attrib()[i])
        } 
      }} 
    numattr})

# betweenness centrality of all nodes (for sizing menu)
nodebetw <- reactive({
  if(!is.network(nw())){return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
    cmode <- 'directed'
  } else {
    gmode <- 'graph'
    cmode <- 'undirected'
  }
  betweenness(nw(), gmode=gmode, diag=has.loops(nw()),

              cmode=cmode)
})

nodesize <- reactive({
  if(!is.network(nw())){return()}
  nw_var <- nw()
  #scale size of nodes onto range between .7 and 3.5
  minsize <- min(get.vertex.attribute(nw_var,input$sizeby))
  maxsize <- max(get.vertex.attribute(nw_var,input$sizeby))
  if (input$sizeby == '1'){
    size = 1
  } else if (input$sizeby == 'Betweenness'){
    minsize <- min(nodebetw())
    maxsize <- max(nodebetw())
    size = (nodebetw()-minsize)/(maxsize-minsize)*(3.5-.7)+.7
  } else {
    minsize <- min(get.vertex.attribute(nw_var,input$sizeby))
    maxsize <- max(get.vertex.attribute(nw_var,input$sizeby))
    size = (get.vertex.attribute(nw_var,input$sizeby)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }
  size})

#vertex color
vcol <- reactive({
  if(!is.network(nw())){return()}
  nw_var <- nw()
  if(input$colorby ==2){
    vcol <- 2
  } else {
    full_list <- get.vertex.attribute(nw_var,input$colorby)
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
  if(!is.network(nw())){return()}
  nw_var <- nw()
    if(input$colorby == 2){
      legendlabels <- NULL
    }else{
      legendlabels <- sort(unique(get.vertex.attribute(nw_var, input$colorby)))
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

#' ERGM fitting:  
#' `ergm.terms` is a compilation of all the terms entered,
#' which we then use to create a complete formula. 
#' 
#+ eval=FALSE 

#add terms to list as user enters them
observe({
  if(input$addtermButton==0) {return()}
  isolate({
    valsofar <- get('input_termslist',pos='package:base')
    newval <- input$terms
    assign('input_termslist', rbind(valsofar, newval),
           pos='package:base')
    updateTextInput(session, inputId='terms', value='')
  })
})

observe({
  if(input$resetformulaButton==0) {return()}
  isolate({
    assign('input_termslist', list(), pos='package:base')
    updateTextInput(session, inputId='terms', value='')
  })
})

ergm.terms <- reactive({
  nw()
  input$resetformulaButton
  input$addtermButton
  interms <- get('input_termslist', pos='package:base')
  if(length(interms)==0) {return('NA')}
  paste(interms, collapse = '+')
  })

ergm.formula <- reactive({
  if(ergm.terms()=='NA') {return()}
  formula(paste('nw() ~ ',ergm.terms(), sep = ''))})

#' Once we have a formula, creating a model object, checking the goodness of fit
#' and simulating from it is similar to what would be written in the command line,
#' wrapped in a reactive statement.
#+ eval=FALSE 
model1reac <- reactive({
  if(input$fitButton == 0){
    return()
  }
  usingdefault <- isolate(input$controldefault)
  if(usingdefault){
    f <- isolate(ergm(ergm.formula()))
  } else {
    customcontrols <- isolate(paste(input$customMCMCcontrol, sep=","))
    if(customcontrols == ""){
      f <- isolate(ergm(ergm.formula(), control=control.ergm(MCMC.interval=isolate(input$MCMCinterval),
                                                             MCMC.burnin=isolate(input$MCMCburnin),
                                                             MCMC.samplesize=isolate(input$MCMCsamplesize))))
    } else {
      f <- isolate(ergm(ergm.formula(), control=control.ergm(MCMC.interval=isolate(input$MCMCinterval),
                                                             MCMC.burnin=isolate(input$MCMCburnin),
                                                             MCMC.samplesize=isolate(input$MCMCsamplesize),
                                                             eval(parse(text=customcontrols)))))
    }
    
  }
  f
  })

# values$modelstate keeps track of whether model fit is oudated,
# equal to 0 if the nw changes and a new model has not been fit yet
# equal to 1 otherwise
values$modelstate <- 0

#Keep track of saved models
values$modeltotal <- 0
values$modelcoefs <- list()
values$modelformulas <- list()
values$modelfits <- list()

observe({
  if(!is.null(input$savemodelButton)){
    m <- isolate(values$modeltotal)
    if(input$savemodelButton > 0 & m < 5){
      #increment label on save model button
      values$modeltotal <- m+1
    }
  }
})
observe({
  # Add to lists that hold info for each model
  # values$modelcoefs is a list object, each element is a list of 
  # coefficients and stars for a single model
  m <- values$modeltotal
  if(m > 0 & m < 5){
    values$modelcoefs[[m]] <- isolate(ergminfo(model1reac()))
    values$modelformulas[[m]] <- isolate(ergm.terms())
    values$modelfits[[m]] <- isolate(model1reac())
  }
})
observe({
  #disable savemodelButton before any models have been saved, 
  #after 5 models have been saved, or after the network changes
  if(input$fitButton == 0){
    updateButton(session, 'savemodelButton', disabled=TRUE)
  } else if(values$modeltotal == 5){
    updateButton(session, 'savemodelButton', disabled=TRUE)
  } else if(values$modelstate == 0){
    updateButton(session, 'savemodelButton', disabled=TRUE)
  } else {
    updateButton(session, 'savemodelButton', disabled=FALSE)
  }
})
observe({
  #clear saved models after button click
  if(input$clearmodelButton==0){
    return()
  }
  values$modeltotal<-isolate(0)
  values$modelcoefs <- list()
  values$modelformulas <- list()
  values$modelfits <- list()
})
observe({
  #clear saved models when network changes
  if(values$modelstate==0){
    values$modeltotal<-isolate(0)
    values$modelcoefs <- list()
    values$modelformulas <- list()
    values$modelfits <- list()
  }
})

model1gof <- reactive({
  input$gofButton
  if(input$gofterm == ''){
    #use default gof formula
    model1gof <- gof(model1reac())
  } else {
    gofform <- formula(paste('model1reac() ~ ', input$gofterm, sep = ''))
    model1gof <- gof(gofform)
  }
  model1gof})


model1simreac <- reactive({
  input$simButton
  if(input$simcontroldefault){
    s<-isolate(simulate(model1reac(), nsim = input$nsims))
  } else {
    customcontrols <- isolate(paste(input$simcustomMCMCcontrol, sep=","))
    if(customcontrols == ""){
      s<-isolate(simulate(model1reac(), nsim = input$nsims,
                       control=control.simulate.ergm(MCMC.burnin=input$simMCMCburnin,
                                                    MCMC.interval=input$simMCMCinterval)))
    } else {
      s<-isolate(simulate(model1reac(), nsim = input$nsims,
                       control=control.simulate.ergm(MCMC.burnin=input$simMCMCburnin,
                                                    MCMC.interval=input$simMCMCinterval,
                                                    eval(parse(text=customcontrols)))))
    }
  }
  s
  })

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
  nw_var <- nw()
  #scale size of nodes onto range between .7 and 3.5
  if (input$sizeby2 == '1'){
    size = 1
  } else if (input$sizeby2 == "Betweenness"){
    minsize <- min(nodebetw2())
    maxsize <- max(nodebetw2())
    size <- (nodebetw2()-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  } else {
    minsize <- min(get.vertex.attribute(nw_var,input$sizeby2))
    maxsize <- max(get.vertex.attribute(nw_var,input$sizeby2))
    if(input$nsims==1){
    size <- (get.vertex.attribute(model1simreac(),input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }else{
    size <- (get.vertex.attribute(model1simreac()[[input$thissim]],input$sizeby2)-minsize)/(maxsize-minsize)*(3.5-.7)+.7 
  }}
  size})

vcol2 <- reactive({
  if(!is.network(nw())){return()}
  input$simButton
  isolate(nsim <- input$nsims)
  if(!is.null(input$colorby2)){
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
  }
  vcol
})

legendlabels2 <- reactive({
  nw_var <- nw()
  if(!is.null(input$colorby2)){
  if(input$colorby2 == 2){
    legendlabels <- NULL
  }else{
    legendlabels <- sort(unique(get.vertex.attribute(nw_var, input$colorby2)))
    if(is.element("Other", legendlabels)){
      legendlabels <- legendlabels[-which(legendlabels=="Other")]
      legendlabels <- c(legendlabels, "Other")
    }
  }
  }
  legendlabels
})

legendfill2 <- reactive({
  if(!is.null(input$colorby2)){
  if(input$colorby2 == 2){
    legendfill <- NULL
  } else {
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    legendfill <- adjustcolor(pal, alpha.f = input$transp2)
  }
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

observe({
  if(input$symmetrize=="Do not symmetrize"){
    updateButtonGroup(session, 'aftersymm', disabled=TRUE)
  } else {
    updateButtonGroup(session, 'aftersymm', disabled=FALSE)
  }
})

output$newattrname <- renderPrint({
  if(!is.null(input$newattrvalue)){
      cat(newattrnamereac())}
})

# output$modifyattrchooser <- renderUI({
#   if(!is.network(nwmid())) {return()}
#   vattr <- list.vertex.attributes(nwmid())
#   eattr <- list.edge.attributes(nwmid())
#   attrlist <- c(vattr, eattr)
#   selectInput('modifyattrs', label=NULL, choices=attrlist,
#               selectize=FALSE)
# })


#summary of network attributes
output$nwsum <- renderPrint({
  if (is.null(nw())){
    return(cat('NA'))
  }
  nw_var <- nw()
  if (class(nw_var)!="network"){
    return(cat(nw_var))
  }
  return(nw_var)
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
  if (!is.network(nw())){
    return(cat('NA'))
  }
  nw_var <- nw()
  return(nw_var)
})

output$dynamiccolor <- renderUI({
  selectInput('colorby',
              label = 'Color nodes according to:',
              c('None' = 2, attrib()),
              selectize = FALSE)
})
outputOptions(output,'dynamiccolor',suspendWhenHidden=FALSE, priority=10)

observe({
  if(length(legendlabels())>9){
    createAlert(session, inputId = "colorwarning",
                title=NULL, 
                message="Warning: Colors get recycled for attributes with more than nine levels.",
                type="warning", dismiss=TRUE, 
                block=FALSE, append=FALSE)
  }
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
  if (!is.network(nw())){
    return()
  }
  input$plottabs
  input$rawdatafile
  input$samplenet
  
  nw_var <- nw()
  color <- adjustcolor(vcol(), alpha.f = input$transp)
  par(mar = c(0, 0, 0, 0))
  plot.network(nw_var, coord = coords(), 
               displayisolates = input$iso, 
               displaylabels = input$vnames, 
               vertex.col = color,
               vertex.cex = nodesize())
  if(input$colorby != 2){
    legend('bottomright', title = input$colorby, legend = legendlabels(), fill = legendfill(),
           bty='n')
  }
})

output$nwplotdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_plot.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=10)
    nw_var <- nw()
    color <- adjustcolor(vcol(), alpha.f = input$transp)
    plot.network(nw_var, coord = coords(), 
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
  if(!is.network(nw())){
    return()
  }
  if(is.directed(nw())){
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='digraph',
                    diag=has.loops(nw()))
  } else {
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='graph',
                    diag=has.loops(nw()))
  }
  samples
})

bernoullisamples <- reactive({
  if(!is.network(nw())){
    return()
  }
  density <- gden(nw())
  if(is.directed(nw())){
    samples <- rgraph(n=nodes(), m=50, mode='digraph', tprob=density,
                      diag=has.loops(nw()))
  } else {
    samples <- rgraph(n=nodes(), m=50, mode='graph', tprob=density,
                      diag=has.loops(nw()))
  }
  samples
})

#DEGREE DISTRIBUTION

output$dynamiccolor_dd <- renderUI({
  selectInput('colorby_dd',
              label = 'Color bars according to:',
              c('None', menuattr()),
              selected = 'None',
              selectize = FALSE)
})
outputOptions(output,'dynamiccolor_dd',suspendWhenHidden=FALSE, priority=10)

observe({
  if(is.network(nw())){
    if(!is.directed(nw())){
      disableWidget('cmode', session, TRUE)
    } else {
      disableWidget('cmode', session, FALSE)
    }
  }
})

dd_plotdata <- reactive({
  if(!is.network(nw())){
    return()
  }
  if(is.directed(nw())){
    gmode <- "digraph"
  } else {
    gmode <- "graph"
  }
  if(has.loops(nw())){
    diag <- TRUE
  } else {
    diag <- FALSE
  }
  deg <- degree(nw(), gmode=gmode, cmode=input$cmode, diag=diag)
  data <-tabulate(deg)
  data <- append(data,sum(deg==0),after=0)
  maxdeg <- max(deg)
  names(data) <- paste(0:maxdeg)
  
  #for color-coded bars
  if(!is.null(input$colorby_dd) & input$colorby_dd != "None"){
    if(is.directed(nw())){
      if(input$cmode=='indegree'){
        data <- summary(nw() ~ idegree(0:maxdeg, input$colorby_dd))
      } else if(input$cmode=='outdegree'){
        data <- summary(nw() ~ odegree(0:maxdeg, input$colorby_dd))
      } else {
        return('Cannot color code a directed graph using total degree.')
      }
    } else {
      data <- summary(nw() ~ degree(0:maxdeg, input$colorby_dd))
    }
    data <- t(matrix(data,nrow=maxdeg+1))
    colnames(data) <- 0:maxdeg
  }
  data
})

observe({
  if(!is.null(input$colorby_dd)){
    if(input$colorby_dd != "None"){
      if(dim(dd_plotdata())[1]>9){
        createAlert(session, inputId = "colorwarning_dd",
                    title=NULL, 
                    message="Warning: Colors get recycled for attributes with more than nine levels.",
                    type="warning", dismiss=TRUE, 
                    block=FALSE, append=FALSE)
      }
    }
  }
})

dd_uniformoverlay <- reactive({
  if(!is.network(nw())){
    return()
  }
  reps <- 50 #number of draws
  if(is.directed(nw())){
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
  if(!is.network(nw())){
    return()
  }
  reps = 50
  density <- gden(nw())
  if(is.directed(nw())){
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
  if(!is.network(nw())){
    return()
  }
  input$plottabs
  input$rawdatafile
  input$samplenet
  
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
    color[is.na(color)] <- brewer.pal(9, "Blues")
    ltext <- sort(unique(get.vertex.attribute(nw(),input$colorby_dd)))
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
    pdf(file=file, height=8, width=12)
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
        color[is.na(color)] <- brewer.pal(9, "Blues")
        ltext <- sort(unique(get.vertex.attribute(nw(),input$colorby_dd)))
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

gdata <- reactive({
  g <- geodist(nw(), inf.replace=NA, count.paths=FALSE)
  gdata <- tabulate(g$gdist)
  g$gdist[is.na(g$gdist)] <- Inf
  gdata <- append(gdata, sum(g$gdist == Inf))
  maxgeo <- length(gdata)-1
  names(gdata) <- c(paste(1:maxgeo), "Inf")
  gdata
})

gd_uniformoverlay <- reactive({
  if(!is.network(nw())){
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
  if(!is.network(nw())){
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
  if(!is.network(nw())){
    return()
  }
  input$plottabs
  input$rawdatafile
  input$samplenet
  
  gdata <- gdata()
  maxgeo <- length(gdata)-1
  
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
  
  if(input$excludeInfs){
    gdata <- gdata[1:maxgeo_total]
    bern_means <- bern_means[1:maxgeo_total]
    bern_upperline <- bern_upperline[1:maxgeo_total]
    bern_lowerline <- bern_lowerline[1:maxgeo_total]
    unif_means <- unif_means[1:maxgeo_total]
    unif_upperline <- unif_upperline[1:maxgeo_total]
    unif_lowerline <- unif_lowerline[1:maxgeo_total]
  }
  #get maximums to set y limits
  ylimit <- max(gdata, bern_upperline, unif_upperline)
  
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
    pdf(file=file, height=8, width=12)

    gdata <- gdata()
    maxgeo <- length(gdata)-1
    
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
    
    if(input$excludeInfs){
      gdata <- gdata[1:maxgeo_total]
      bern_means <- bern_means[1:maxgeo_total]
      bern_upperline <- bern_upperline[1:maxgeo_total]
      bern_lowerline <- bern_lowerline[1:maxgeo_total]
      unif_means <- unif_means[1:maxgeo_total]
      unif_upperline <- unif_upperline[1:maxgeo_total]
      unif_lowerline <- unif_lowerline[1:maxgeo_total]
    }
    
    #get maximums to set y limits
    ylimit <- max(gdata, bern_upperline, unif_upperline)
    
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

  output$infsummary <- renderPrint({
    options(digits=3)
    gdata <- gdata()
    cols <- length(gdata)
    obs <- gdata[cols]
    unif_means <- gd_uniformoverlay()[[1]]
    unif_sd <- gd_uniformoverlay()[[2]]
    bern_means <- gd_bernoullioverlay()[[1]]
    bern_sd <- gd_bernoullioverlay()[[2]]
    
#     v<-data.frame(Obs=paste(obs), CUG=paste(unif_means[cols],"(",round(unif_sd[cols], digits=2),")",sep=""),
#                BRG=paste(bern_means[cols],"(",round(bern_sd[cols], digits=2),")", sep=""),
#                row.names="Infs:")
#     format(v, justify="centre")
    v <- c(paste0(unif_means[cols],"(",round(unif_sd[cols], digits=2),")"),
           paste0(bern_means[cols],"(",round(bern_sd[cols], digits=2),")"))
    cat(format("",width=4),format("Observed",width=8),format(c("CUG","BRG"),width=11,justify="centre"),"\n")
    cat(format("Infs:",width=3,justify="centre"),format(paste(obs),width=8,justify="centre"),
        format(v,width=10,justify="centre"))
  })

#MORE

observe({
  if(input$plottabs == "More"){
    updateTabsetPanel(session, 'displaytabs', selected="Network Summary")
  }
})

# FUTURE: will be able to subset data
# output$subsetting <- renderUI({
#   if(class(nw())!='network'){
#     return()
#   }
#   selectInput('subsetattr', label=NULL,
#               choices = c('None', menuattr()), selectize = FALSE)
# })
# outputOptions(output,'subsetting',suspendWhenHidden=FALSE)
# 
# output$subsetting2 <- renderUI({
#   if(class(nw())!='network' | input$subsetattr == "None"){
#     return()
#   }
#   choices <- sort(unique(get.vertex.attribute(nw(),input$subsetattr)))
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
  if(!is.network(nw())) {return()}
  if(!is.null(input$mixmx)){
  mixingmatrix(nw(), input$mixmx)}
})
outputOptions(output,'mixingmatrix',suspendWhenHidden=FALSE)

output$gden <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  gden(nw(), diag=has.loops(nw()), mode=gmode)
})
outputOptions(output,'gden',suspendWhenHidden=FALSE)

output$grecip <- renderText({
  if(!is.network(nw())) {return()}
  if(input$grecipmeas == ''){
    return()
  }
  try(grecip(nw(), measure=input$grecipmeas))
})
outputOptions(output,'grecip',suspendWhenHidden=FALSE)

output$gtrans <- renderText({
  if(!is.network(nw())) {return()}
  if(input$gtransmeas == ''){
    return()
  }
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  try(gtrans(nw(), diag=has.loops(nw()), mode=gmode,
         measure=input$gtransmeas))
})
outputOptions(output,'gtrans',suspendWhenHidden=FALSE)

output$gdeg <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- NULL
  cmode <- input$gdegcmode
  if(cmode == 'total'){
    cmode <- 'freeman'
  }
  try(d <- centralization(nw(), degree, mode=gmode, diag=has.loops(nw()),
              cmode=cmode))
  d
})
outputOptions(output,'gdeg',suspendWhenHidden=FALSE)

output$gbetw <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- NULL
  try(b <- centralization(nw(), betweenness, mode=gmode, diag=has.loops(nw()),
                   cmode=input$gbetwcmode))
  b
})
outputOptions(output,'gbetw',suspendWhenHidden=FALSE)

output$gclose <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- NULL
  try(
    c <- centralization(nw(), closeness, mode=gmode, diag=has.loops(nw()),
                   cmode=input$gclosecmode))
  c
})
outputOptions(output,'gclose',suspendWhenHidden=FALSE)

output$gstress <- renderText({
  if(!is.network(nw())){ return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- NULL
  try(s <- centralization(nw(), stresscent, mode=gmode, diag=has.loops(nw()),
                  cmode=input$gstresscmode))
  s
})
outputOptions(output,'gstress',suspendWhenHidden=FALSE)

output$ggraphcent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- NULL
  try(g <- centralization(nw(), graphcent, mode=gmode, diag=has.loops(nw()),
                 cmode=input$ggraphcentcmode))
  g
})
outputOptions(output,'ggraphcent',suspendWhenHidden=FALSE)

output$gevcent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- NULL
  try(e <- centralization(nw(), evcent, mode=gmode, diag=has.loops(nw())))
  e
})
outputOptions(output,'gevcent',suspendWhenHidden=FALSE)

output$ginfocent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-NULL
  try({
    i <- centralization(nw(), infocent, mode=gmode, diag=has.loops(nw()),
                  cmode=input$ginfocentcmode)})
  i
})
outputOptions(output,'ginfocent',suspendWhenHidden=FALSE)

output$ndeg <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  cmode <- input$ndegcmode
  if(cmode == 'total'){
    cmode <- 'freeman'
  }
  d <- NULL
  try(d <- degree(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
         cmode=cmode))
  d
})
outputOptions(output,'ndeg',suspendWhenHidden=FALSE)

output$ndegmin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(nw(), gmode=gmode, diag=has.loops(nw()),
              cmode=input$ndegcmode)
  min(d)
})
outputOptions(output,'ndegmin',suspendWhenHidden=FALSE)

output$ndegmax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(nw(), gmode=gmode, diag=has.loops(nw()),
              cmode=input$ndegcmode)
  max(d)
})
outputOptions(output,'ndegmax',suspendWhenHidden=FALSE)

output$nbetw <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- NULL
  try(b <- betweenness(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
                   cmode=input$nbetwcmode))
  b
})
outputOptions(output,'nbetw',suspendWhenHidden=FALSE)

output$nbetwmin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(nw(), gmode=gmode, diag=has.loops(nw()),
                   cmode=input$nbetwcmode)
  min(b)
})
outputOptions(output,'nbetwmin',suspendWhenHidden=FALSE)

output$nbetwmax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(nw(), gmode=gmode, diag=has.loops(nw()),
                   cmode=input$nbetwcmode)
  max(b)
})
outputOptions(output,'nbetwmax',suspendWhenHidden=FALSE)

output$nclose <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- NULL
  try(
    c <- closeness(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
                   cmode=input$nclosecmode))
  c
})
outputOptions(output,'nclose',suspendWhenHidden=FALSE)

output$nclosemin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(nw(), gmode=gmode, diag=has.loops(nw()),
                 cmode=input$nclosecmode)
  min(c)
})
outputOptions(output,'nclosemin',suspendWhenHidden=FALSE)

output$nclosemax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(nw(), gmode=gmode, diag=has.loops(nw()),
                 cmode=input$nclosecmode)
  max(c)
})
outputOptions(output,'nclosemax',suspendWhenHidden=FALSE)

output$nstress <- renderText({
  if(!is.network(nw())){ return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- NULL
  try(s <- stresscent(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
                  cmode=input$nstresscmode))
  s
})
outputOptions(output,'nstress',suspendWhenHidden=FALSE)

output$nstressmin <- renderText({
  if(!is.network(nw())){ return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(nw(), gmode=gmode, diag=has.loops(nw()),
                  cmode=input$nstresscmode)
  min(s)
})
outputOptions(output,'nstressmin',suspendWhenHidden=FALSE)

output$nstressmax <- renderText({
  if(!is.network(nw())){ return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(nw(), gmode=gmode, diag=has.loops(nw()),
                  cmode=input$nstresscmode)
  max(s)
})
outputOptions(output,'nstressmax',suspendWhenHidden=FALSE)

output$ngraphcent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- NULL
  try(g <- graphcent(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
                  cmode=input$ngraphcentcmode))
  g
})
outputOptions(output,'ngraphcent',suspendWhenHidden=FALSE)

output$ngraphcentmin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(nw(), gmode=gmode, diag=has.loops(nw()),
                 cmode=input$ngraphcentcmode)
  min(g)
})
outputOptions(output,'ngraphcentmin',suspendWhenHidden=FALSE)

output$ngraphcentmax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(nw(), gmode=gmode, diag=has.loops(nw()),
                 cmode=input$ngraphcentcmode)
  max(g)
})
outputOptions(output,'ngraphcentmax',suspendWhenHidden=FALSE)

output$nevcent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- NULL
  try(e <- evcent(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw())))
  e
})
outputOptions(output,'nevcent',suspendWhenHidden=FALSE)

output$nevcentmin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(nw(), gmode=gmode, diag=has.loops(nw()))
  min(e)
})
outputOptions(output,'nevcentmin',suspendWhenHidden=FALSE)

output$nevcentmax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(nw(), gmode=gmode, diag=has.loops(nw()))
  max(e)
})
outputOptions(output,'nevcentmax',suspendWhenHidden=FALSE)

output$ninfocent <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-''
  try({
    i <- infocent(nw(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nw()),
                   cmode=input$ninfocentcmode)})
  i
})
outputOptions(output,'ninfocent',suspendWhenHidden=FALSE)

output$ninfocentmin <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i<-''
  try({
    i <- infocent(nw(), gmode=gmode, diag=has.loops(nw()),
                  cmode=input$ninfocentcmode)
    i<-min(i)})
  i
})
outputOptions(output,'ninfocentmin',suspendWhenHidden=FALSE)

output$ninfocentmax <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i <- ''
  try({
    i <- infocent(nw(), gmode=gmode, diag=has.loops(nw()),
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
#' Coming soon: term documentation.


#+ eval=FALSE
# UNCOMMENT AFTER RELEASE FOR TERM DOCUMENTATION
# output$listofterms <- renderUI({
#   if(!is.network(nw())){
#     return()
#   }
#   if(input$matchingorall == 'All terms'){
#     current.terms <- unlist(allterms)
#   } else {
#     matchterms <- search.ergmTerms(net=nw())
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

observe({
  if(input$controldefault){
    updateNumericInput(session, "MCMCinterval", value=1024)
    #burn-in gets updated separately, to be 16*interval
    updateNumericInput(session, "MCMCsamplesize", value=1024)
    disableWidget("MCMCinterval", session, disabled=TRUE)
    disableWidget("MCMCburnin", session, disabled=TRUE)
    disableWidget("MCMCsamplesize", session, disabled=TRUE)
    disableWidget("customMCMCcontrol", session, disable=TRUE)
  } else {
    disableWidget("MCMCinterval", session, disabled=FALSE)
    disableWidget("MCMCburnin", session, disabled=FALSE)
    disableWidget("MCMCsamplesize", session, disabled=FALSE)
    disableWidget("customMCMCcontrol", session, disable=FALSE)
  }
})

observe({
  input$MCMCinterval
  updateNumericInput(session, "burnin", value=16*input$MCMCinterval)
})


#' Below we output the current formulation of the ergm 
#' model so the user can clearly see how their menu selections change the model.
#' Since `ergm.terms()` is a reactive object, it will automatically update when
#' the user clicks on menu options.
#'  
#+ fitmodel2, eval=FALSE
output$currentdataset1 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  cat(isolate(nwname()))
})

output$checkterms1 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
  cat(ergm.terms())
})

output$prefitsum <- renderPrint({
  if(!is.network(nw()) | length(input$terms)==0){
    return(cat('NA'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
  options(width=150)
  summary(ergm.formula())
})

output$savemodel <- renderUI({
  m <- values$modeltotal
  bsActionButton('savemodelButton',label=paste0('Save Current Model (',m,'/5)'),
                 block=FALSE)
})
outputOptions(output,'savemodel',suspendWhenHidden=FALSE)

observe({
  input$fitButton
  values$modelstate <- 1  #modelfit is up to date
})
observe({
  nw()
  values$modelstate <- 0 #modelfit is outdated when nw changes
})

output$modelfit <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  if (values$modelstate == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  model1reac()
})

output$modelfitsum <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  if (values$modelstate == 0){
    return(cat('After adding terms to the formula, click "Fit Model" above.'))
  }
  summary(model1reac())
})

output$modelcomparison <- renderPrint({
  x <- values$modelcoefs
  if(length(x)==0){return(cat(""))}
  model.comparison(x)
})

output$modelcompdownload <- downloadHandler(
  filename = function() {paste0(nwname(),"_modelcomparison.csv")},
  contentType = "text/csv",
  content = function(file) {
    x <- values$modelcoefs
    x <- model.comparison(x)
    write.csv(x, file)
  }
)

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
#' in a reactive object, it gets called in both the plot output element and summary
#' output element.
#' 
#+ eval=FALSE


output$uichoosemodel3 <- renderUI({
  n <- values$modeltotal
  if(n == 0){
    inlineSelectInput("choosemodel3",label=NULL,
                choices=c("Current"),
                selectize=FALSE)
  } else {
    inlineSelectInput("choosemodel3",label=NULL,
                choices=c(paste0("Model",1:n)),
                selectize=FALSE)
  }
})
outputOptions(output,"uichoosemodel3",suspendWhenHidden=FALSE)

output$checkterms3 <- renderPrint({
  if(is.null(nw())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  mod <- input$choosemodel3
  if(mod=="Current"){
    cat(isolate(ergm.terms()))
  } else {
    mod <- as.numeric(substr(mod,6,6))
    cat(values$modelformulas[[mod]])
  }
})

output$currentdataset3 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

output$diagnosticsplot <- renderPlot({
  if(ergm.terms()=="NA"){
    return()
  }
  mod <- input$choosemodel3
  if(mod=="Current"){
    mod <- model1reac()
  } else {
    modn <- as.numeric(substr(mod,6,6))
    mod <- values$modelfits[[modn]]
  }
  vpp <- length(mod$coef)
  tryCatch(
    mcmc.diagnostics(mod, vars.per.page = vpp),
    error = function(e) cat("MCMC was not run or MCMC sample was not stored."))
})

output$mcmcplotdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_mcmc.pdf',sep='')},
  content = function(file){
    mod <- input$choosemodel3
    if(mod=="Current"){
      mod <- model1reac()
    } else {
      modn <- as.numeric(substr(mod,6,6))
      mod <- values$modelfits[[modn]]
    }
    vpp <- length(mod$coef)
    pdf(file=file, height=vpp*4/3, width=10)
    tryCatch(
      mcmc.diagnostics(model1reac(), vars.per.page = vpp),
      error = function(e) cat("MCMC was not run or MCMC sample was not stored."))
    dev.off()
  }
)

output$diagnosticsplotspace <- renderUI({
  if(input$fitButton == 0 | ergm.terms()=="NA"){
    return()
  }
  mod <- input$choosemodel3
  if(mod=="Current"){
    mod <- model1reac()
  } else {
    modn <- as.numeric(substr(mod,6,6))
    mod <- values$modelfits[[modn]]
  }
  vpp <- length(mod$coef)
  plotOutput('diagnosticsplot', height = vpp*400/2)
})

output$diagnostics <- renderPrint({
  if(input$fitButton == 0 | ergm.terms()=="NA"){
    return()
  }
  mod <- input$choosemodel3
  if(mod=="Current"){
    mod <- model1reac()
  } else {
    modn <- as.numeric(substr(mod,6,6))
    mod <- values$modelfits[[modn]]
  }
  isolate(tryCatch(
    mcmc.diagnostics(mod),
    error = function(e) cat("MCMC was not run or MCMC sample was not stored.")))
})
outputOptions(output, 'diagnostics', suspendWhenHidden=FALSE)



#' **Diagnostics - Goodness of Fit**
#' 
#' Again, we output the current dataset and the ergm formula for the user to verify.
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
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

output$uichoosemodel2 <- renderUI({
  n <- values$modeltotal
  if(n == 0){
    inlineSelectInput("choosemodel2",label=NULL,
                      choices=c("Current"),
                      selectize=FALSE)
  } else {
    inlineSelectInput("choosemodel2",label=NULL,
                      choices=c(paste0("Model",1:n)),
                      selectize=FALSE)
  }
})
outputOptions(output,"uichoosemodel2",suspendWhenHidden=FALSE)
#formula only updates after fitButton has been clicked
output$checkterms2 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  mod <- input$choosemodel2
  if(mod=="Current"){
    cat(isolate(ergm.terms()))
  } else {
    mod <- as.numeric(substr(mod,6,6))
    cat(values$modelformulas[[mod]])
  }
})

#state$gof will toggle between two states, depending on
#if gof plots are outdated compared to current ergm formula
state <- reactiveValues(gof = 0)

observe({
  input$fitButton
  state$gof <- 0 #gof plots are outdated
})

observe({
  input$gofButton
  state$gof <- 1 #gof plots are up to date
})

output$gofsummary <- renderPrint({
  if (input$gofButton == 0){
    return()
  }
  s <- isolate({model1gof()})
  if(state$gof == 0){
    return()
  }
  return(s)
  })
outputOptions(output, 'gofsummary', suspendWhenHidden=FALSE)

output$gofplot <- renderPlot({
  input$gofButton
  if(state$gof == 0){
    return()
  }
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
  if(state$gof == 0){
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
output$uichoosemodel4 <- renderUI({
  n <- values$modeltotal
  if(n == 0){
    inlineSelectInput("choosemodel4",label=NULL,
                      choices=c("Current"),
                      selectize=FALSE)
  } else {
    inlineSelectInput("choosemodel4",label=NULL,
                      choices=c(paste0("Model",1:n)),
                      selectize=FALSE)
  }
})
outputOptions(output,"uichoosemodel4",suspendWhenHidden=FALSE)

output$checkterms4 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  cat(isolate(ergm.terms()))
})
output$currentdataset4 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

observe({
  if(input$simcontroldefault){
    updateNumericInput(session, "simMCMCinterval", value=1024)
    #burn-in gets updated separately, to be 16*interval
    disableWidget("simMCMCinterval", session, disabled=TRUE)
    disableWidget("simMCMCburnin", session, disabled=TRUE)
    disableWidget("simcustomMCMCcontrol", session, disable=TRUE)
  } else {
    disableWidget("simMCMCinterval", session, disabled=FALSE)
    disableWidget("simMCMCburnin", session, disabled=FALSE)
    disableWidget("simcustomMCMCcontrol", session, disable=FALSE)
  }
})

observe({
  input$simMCMCinterval
  updateNumericInput(session, "simMCMCburnin", value=16*input$simMCMCinterval)
})

output$simnum <- renderText({
  input$simButton
  n <- isolate(input$nsims)
  n
})
outputOptions(output, 'simnum', suspendWhenHidden=FALSE)

output$simsummary <- renderPrint({
  if (input$simButton == 0){
    return(cat(''))
  }
  sim <- isolate(model1simreac())
  n <- isolate(input$nsims)
  sum <- list()
  sum[1] <- paste(" Number of Networks: ",n, "\n")
  sum[2] <- paste("Model: ", nwname(),' ~ ',ergm.terms(), "\n")
  sum[3] <- paste("Reference: ", format(attr(sim,'reference')), "\n")
  sum[4] <- paste("Constraints: ", format(attr(sim, 'constraints')), "\n")
  sum[5] <- paste("Parameters: \n")
  cat(unlist(sum))
})

output$simcoef <- renderPrint({
  if (input$simButton == 0){
    return(cat(''))
  }
  sim <- isolate(model1simreac())
  c <- attr(sim, 'coef')
  c <- cbind(format(names(c)),format(c, digits=3))
  write.table(c, quote=FALSE, row.names=FALSE, col.names=FALSE)
})

output$simstatslabel <- renderPrint({
  if (input$simButton == 0){
    return(cat(''))
  }
  cat(' Stored network statistics:')
})

output$simstats <- renderPrint({
  if (input$simButton == 0){
    return(cat(''))
  }
  sim <- isolate(model1simreac())
  m <- format(t(attr(sim,'stats')))
  m <- cbind(format(rownames(m)),m)
  write.table(m, quote=F, row.names=F, col.names=F)
})

output$simstats2 <- renderPrint({
  if(input$simButton == 0){
    return()
  }
  sim <- isolate(model1simreac())
  m <- attr(sim,'stats')
  mat <- cbind(apply(m,2,min),apply(m,2,max),apply(m,2,IQR),
               apply(m,2,mean),apply(m,2,sd))
  colnames(mat) <- c("min","max","IQR","mean","SD")
  mat
})

output$simstatsdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_simstats.csv',sep='')},
  contentType = "text/csv",
  content = function(file) {
    x<-attr(model1simreac(),"stats")
    write.csv(x, file)
  }
  )

output$simstatsplot <- renderPlot({
  if(input$simButton==0){
    return()
  }
  sim <- isolate(model1simreac())
  simstats <- attr(sim,'stats')
  targetstats <- summary(ergm.formula())
  matplot(1:nrow(simstats), simstats, pch=c(1:8),
          col=c('red', 'blue', 'green3', 'cyan', 'magenta3',
                'orange', 'black', 'grey', 'yellow'),
          xlab="Simulations",ylab="")
  abline(h=c(targetstats),
         col=c('red', 'blue', 'green3', 'cyan', 'magenta3',
               'orange', 'black', 'grey', 'yellow'))
})

output$simstatslegend <- renderPlot({
  if(input$simButton==0){
    return()
  }
  sim <- isolate(model1simreac())
  termnames <- colnames(attr(sim,'stats'))
  color <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'orange', 'black', 'grey', 'yellow')
  plot.new()
  par(xpd=TRUE)
  legend('topleft', inset=c(-.15,0), 
         legend=c(termnames, paste("target stat:",termnames)),
         pch=c(1:length(termnames), rep(NA,length(termnames))),
         lty=c(rep(NA,length(termnames)),rep(1,length(termnames))),
         col=rep(color[1:length(termnames)],2))
})

output$simstatsplotdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_simstatsplot.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=8, width=15)
    sim <- isolate(model1simreac())
    simstats <- attr(sim,'stats')
    termnames <- colnames(attr(sim,'stats'))
    targetstats <- summary(ergm.formula())
    color <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
               'orange', 'black', 'grey', 'yellow')
    par(mar=c(5,4,4,12)) #increase margin on right to accommodate legend
    matplot(1:nrow(simstats), simstats, pch=c(1:8),
            col=color,
            xlab="Simulations",ylab="")
    abline(h=c(targetstats),
           col=color)
    par(xpd=TRUE)
    legend('topright', inset=c(-.2,0),
           legend=c(termnames, paste("target stat:",termnames)),
           pch=c(1:length(termnames), rep(NA,length(termnames))),
           lty=c(rep(NA,length(termnames)),rep(1,length(termnames))),
           col=rep(color[1:length(termnames)],2))
    dev.off()
  }
)

output$simsummary2 <- renderPrint({
  if (input$simButton == 0){
    return(cat(''))
  }
  sim <- isolate(model1simreac())
  sim
})

output$dynamiccolor2 <- renderUI({
  selectInput('colorby2',
              label = 'Color nodes according to:',
              c('None' = 2, attrib()),
              selected = 2,
              selectize = FALSE)
})
outputOptions(output,'dynamiccolor2',suspendWhenHidden=FALSE, priority=10)

observe({
  if(length(legendlabels2())>9){
    createAlert(session, inputId = "colorwarning2",
                title=NULL, 
                message="Warning: Colors get recycled for attributes with more than nine levels.",
                type="warning", dismiss=TRUE, 
                block=FALSE, append=FALSE)
  }
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
  nw_var <- nw()
  nsims <- isolate(input$nsims)
  model1sim <- isolate(model1simreac()) 
  
  #can't plot simulation number greater than total sims
  if(input$thissim > nsims){
    return()
  } 
  
  color <- adjustcolor(vcol2(), alpha.f = input$transp2)
  par(mar = c(0, 0, 0, 0))
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
      legend('bottomright', title=input$colorby2, legend = legendlabels2(), fill = legendfill2(),
             bty='n')
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

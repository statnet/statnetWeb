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
source("chooser.R")


#' Loading data and assigning variables outside of the call to `shinyServer`
#' saves time because this code will not get re-run.These don't depend on any user input
#' and will never change value, so they can be global variables (common to all
#' shiny sessions). 
#+ eval=FALSE 

data(faux.mesa.high)
data(florentine)
data(sampson)

#create a list of unique term names
allterms <- search.ergmTerms()
inds <- gregexpr(pattern='\\(', allterms)
for(i in 1:length(allterms)){
  allterms[i] <- substr(allterms[[i]], start=1, stop=inds[[i]][1]-1)
}
allterms <- unique(allterms)

   


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
      try(nw <- network(read.table(paste(filepath)),
                        directed=input$dir, loops=input$loops,
                        multiple=input$multiple, bipartite=input$bipartite,
                        matrix.type=input$matrixtype))
    }
    
  } else if(input$filetype ==5){
    nw <- eval(parse(text = input$samplenet))
    if(!is.element('bipartite',names(nw$gal))){
      set.network.attribute(nw,'bipartite',FALSE)
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
    
  }
})

#' TO KEEP LIST OF ALL NEW ATTRIBUTES FROM USER
#' can't use global variables because they are common to all 
#' sessions using the app, instead created per session variables 
#' inside shinyServer
#' 
#' 
#+ eval=FALSE

#add vertex attributes
observe({
  if(input$newattrButton == 0) return()
  isolate({
      if(input$newattrtype == 'vertex attribute'){
          path <- input$newattrvalue[1,4]
          objname <- load(paste(path))
          newval <- get(objname)
          namesofar <- get("v_attrNamesToAdd", pos="package:base")
          valsofar <- get("v_attrValsToAdd", pos="package:base")
          
          assign('v_attrNamesToAdd', cbind(namesofar,input$newattrname),
                 pos="package:base")
          assign('v_attrValsToAdd', cbind(valsofar, newval),
                 pos="package:base")
        }
  })
})

#add edge attributes
observe({
  if(input$newattrButton == 0) return()
  isolate({
    if(input$newattrtype == 'edge attribute'){
        path <- input$newattrvalue[1,4]
        objname <- load(paste(path))
        newval <- get(objname)
        namesofar <- get("e_attrNamesToAdd", pos="package:base")
        valsofar <- get("e_attrValsToAdd", pos="package:base")
        
        assign('e_attrNamesToAdd', cbind(namesofar,input$newattrname),
               pos="package:base")
        assign('e_attrValsToAdd', cbind(valsofar, newval),
               pos="package:base")
      }
  })
})

#add edge values
observe({
  if(input$newattrButton == 0) return()
  isolate({
    if(input$newattrtype == 'edge value'){
      path <- input$newattrvalue[1,4]
      objname <- load(paste(path))
      newval <- get(objname)
      namesofar <- get("ev_attrNamesToAdd", pos="package:base")
      valsofar <- get("ev_attrValsToAdd", pos="package:base")
      
      assign('ev_attrNamesToAdd', cbind(namesofar,input$newattrname),
             pos="package:base")
      assign('ev_attrValsToAdd', cbind(valsofar, newval),
             pos="package:base")
    }
  })
})

#update textInput
observe({
  input$newattrButton
  isolate({
    updateTextInput(session, 'newattrname',
                    label='New attribute name', value='')
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
          ev_newval <- ev_attrValsToAdd[,l]
          set.edge.value(nw,ev_newname,ev_newval)
        }
      }
    }
  
    nw
	})

#delete unwanted attributes from this network and use it for future
#calculations
nwreac <- reactive({
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
		plot.network(nwreac())})

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
    if(is.network(nwreac())){        
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
    if(is.network(nwreac())){  
      for(i in 1:length(attr())){
        if(is.numeric(get.vertex.attribute(nwreac(),attr()[i]))){
          numattr <- append(numattr,attr()[i])
        } 
      }} 
    numattr})

  nodesize <- reactive({
    if(!is.network(nwreac())){return()}
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
  if(!is.network(nwreac())){return()}
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
# absdiff.terms <- reactive({
#     aterms <- paste("absdiff('",input$chooseabsdiff,"', pow=",
#                     input$absdiffpow,")", sep="")
#   if(!any(input$terms == 'absdiff')){
#     aterms <- NULL
#   }
#   aterms
# })
# 
# gwesp.terms <- reactive({
#   gterms <- paste("gwesp(",input$choosegwesp,
#                   ", fixed = ",input$fixgwesp,")", sep="")
#   if (!any(input$terms == 'gwesp')){
#     gterms <- NULL
#   }
#   gterms})
# 
# b1degree.terms <- reactive({
#   bterms <- paste("b1degree(",input$chooseb1degree,")", sep="")
#   if(input$chooseb1degree2 != ''){
#     bterms <- paste("b1degree(c(",input$chooseb1degree2,"))", sep="")
#   }
#   if(!any(input$terms == 'b1degree')){
#     bterms <- NULL
#   }
#   bterms})
# 
# b2degree.terms <- reactive({
#   bterms <- paste("b2degree(",input$chooseb2degree,")", sep="")
#   if(input$chooseb2degree2 != ''){
#     bterms <- paste("b2degree(c(",input$chooseb2degree2,"))", sep="")
#   }
#   if(!any(input$terms == 'b2degree')){
#     bterms <- NULL
#   }
#   bterms})
# 
# degree.terms <- reactive({
#   dterms <- paste("degree(",input$choosedegree,")", sep="")
#   if(input$choosedegree2 != ''){
#     dterms <- paste("degree(c(",input$choosedegree2,"))", sep="")
#   }
#   if(!any(input$terms == 'degree')){
#     dterms <- NULL
#   }
#   dterms})
# 
# idegree.terms <- reactive({
#   dterms <- paste("idegree(",input$chooseidegree,")", sep="")
#   if(input$chooseidegree2 != ''){
#     dterms <- paste("idegree(c(",input$chooseidegree2,"))", sep="")
#   }
#   if(!any(input$terms == 'idegree')){
#     dterms <- NULL
#   }
#   dterms})
# 
# odegree.terms <- reactive({
#   dterms <- paste("odegree(",input$chooseodegree,")", sep="")
#   if(input$chooseodegree2 != ''){
#     dterms <- paste("odegree(c(",input$chooseodegree2,"))", sep="")
#   }
#   if(!any(input$terms == 'odegree')){
#     dterms <- NULL
#   }
#   dterms})
# 
# nodefactor.terms <- reactive({
#   if(input$nodefactorbase ==''){
#     nterms <- paste("nodefactor('",input$choosenodefactor,"')", sep="")
#   } else {
#     nterms <- paste("nodefactor('",input$choosenodefactor,"', base=",
#                     input$nodefactorbase,")", sep="")
#   }
#   if(!any(input$terms == 'nodefactor')){
#     nterms <- NULL
#   }
#   nterms
# })
# 
# nodematch.terms <- reactive({
#   middle <- paste(input$choosenodematch, collapse="', '")
#   if(input$nodematchkeep == ''){
#     nterms <- paste("nodematch('",input$choosenodematch,"', diff=", 
#                     input$nodematchdiff,")", sep="")
#   } else {
#     nterms <- paste("nodematch('",input$choosenodematch,"', diff=", 
#                     input$nodematchdiff,", keep=",input$nodematchkeep,")", sep="")
#   }
#   if(!any(input$terms == 'nodematch')){
#     nterms <- NULL
#   }
#   nterms})
# 
# nodemix.terms <- reactive({
#   middle <- paste(input$choosenodemix, collapse="', '")
#   if(input$nodemixbase == ''){
#     nterms <- paste("nodemix(c('",middle,
#                     "'))",sep="")
#   } else {
#     nterms <- paste("nodemix(c('",middle,
#                     "'), base=",input$nodemixbase,")",sep="")
#   }
#   if(!any(input$terms == 'nodemix')){
#     nterms <- NULL
#   }
#   nterms
# })
# 
# nodecov.terms <- reactive({
#     nterms <- paste("nodecov('",input$choosenodecov,
#                     "')",sep="")
#   if(!any(input$terms == 'nodecov')){
#     nterms <- NULL
#   }
#   nterms
# })


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


# output$deleteattrchooser <- renderUI({
#   if(!is.network(nwmid())) return()
#   vattr <- list.vertex.attributes(nwmid())
#   eattr <- list.edge.attributes(nwmid())
#   if(is.element("na",vattr)){
#     vattr <- vattr[-which("na"==vattr)]
#   }
#   if(is.element("na",eattr)){
#     eattr <- eattr[-which("na"==eattr)]
#   }
#   attrlist <- c(vattr, eattr)
#   
#   chooserInput('deleteattrs',"Active Attributes","Inactive Attributes",
#                leftChoices=attrlist, rightChoices=c(),multiple=TRUE)
# })

output$modifyattrchooser <- renderUI({
  if(!is.network(nwmid())) return()
  vattr <- list.vertex.attributes(nwmid())
  eattr <- list.edge.attributes(nwmid())
  attrlist <- c(vattr, eattr)
  selectInput('modifyattrs', label=NULL, choices=attrlist,
              selectize=FALSE)
})


#summary of network attributes
output$nwsum <- renderPrint({
  if (is.null(nwreac())){
    return(cat('NA'))
  }
  nw <- nwreac()
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
  if (!is.network(nwreac())){
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
outputOptions(output,'dynamiccolor',suspendWhenHidden=FALSE)

output$dynamicsize <- renderUI({
  selectInput('sizeby',
              label = 'Size nodes according to:',
              c('None' = 1, numattr()),
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
  if (!is.network(nwreac())){
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

#Data to use for null hypothesis overlays in network plots
uniformsamples <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  if(is.directed(nwreac())){
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='digraph',
                    diag=has.loops(nwreac()))
  } else {
    samples <- rgnm(n=50, nv=nodes(), m=nedges(), mode='graph',
                    diag=has.loops(nwreac()))
  }
  samples
})

bernoullisamples <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  density <- gden(nwreac())
  if(is.directed(nwreac())){
    samples <- rgraph(n=nodes(), m=50, mode='digraph', tprob=density,
                      diag=has.loops(nwreac()))
  } else {
    samples <- rgraph(n=nodes(), m=50, mode='graph', tprob=density,
                      diag=has.loops(nwreac()))
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
  if(!is.network(nwreac())){
    return()
  }
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
  deg <- degree(nwreac(), gmode=gmode, cmode=input$cmode, diag=diag)
  data <-tabulate(deg)
  data <- append(data,sum(deg==0),after=0)
  maxdeg <- max(deg)
  names(data) <- paste(0:maxdeg)
  #for color-coded bars
  
  if(!is.null(input$colorby_dd) & input$colorby_dd != "None"){
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

dd_uniformoverlay <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  if(is.directed(nwreac())){
    deg <- degree(uniformsamples(), g=1:50, gmode='digraph', cmode=input$cmode)
  } else {
    deg <- degree(uniformsamples(), g=1:50, gmode='graph', cmode=input$cmode)
  }
  degreedata <- tabulate(deg)
  degreedata <- append(degreedata, sum(deg==0), after=0)
  names(degreedata) <- paste(0:max(deg))
  degreedata <- degreedata/50
})

dd_bernoullioverlay <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  density <- gden(nwreac())
  if(is.directed(nwreac())){
    deg <- degree(bernoullisamples(), g=1:50, gmode='digraph', cmode=input$cmode)
  } else {
    deg <- degree(bernoullisamples(), g=1:50, gmode='graph', cmode=input$cmode)
  }
  degreedata <- tabulate(deg)
  degreedata <- append(degreedata, sum(deg==0), after=0)
  names(degreedata) <- paste(0:max(deg))
  degreedata <- spline(degreedata/50, n= 4*length(degreedata))
})

output$degreedist <- renderPlot({
  if(!is.network(nwreac())){
    return()
  }
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
          args.legend=legtitle, col=color, ylim=c(0,max(dd_plotdata())+10))
  if(input$uniformoverlay_dd){
    lines(dd_uniformoverlay(),col='firebrick4', lwd=2)
  }
  if(input$bernoullioverlay_dd){
    lines(dd_bernoullioverlay(),col='orangered', lwd=2)
  }
  
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
    if(input$uniformoverlay_dd){
      lines(dd_uniformoverlay(),col='firebrick4', lwd=2)
    }
    if(input$bernoullioverlay_dd){
      lines(dd_bernoullioverlay(),col='orangered', lwd=2)
    }
    dev.off()
})

#GEODESIC DISTRIBUTION

gd_uniformoverlay <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  gd <- geodist(uniformsamples(),inf.replace=0)
  distsum = matrix(0, nrow=nodes(), ncol=nodes())
  for(k in 1:length(gd)){
    distsum <- distsum + gd[[k]]$gdist
  }
  sort(c(distsum/length(gd)), decreasing=TRUE)
})

gd_bernoullioverlay <- reactive({
  if(!is.network(nwreac())){
    return()
  }
  gd <- geodist(bernoullisamples(),inf.replace=0)
  distsum = matrix(0, nrow=nodes(), ncol=nodes())
  for(k in 1:length(gd)){
    distsum <- distsum + gd[[k]]$gdist
  }
  sort(c(distsum/length(gd)), decreasing=TRUE)
})

output$geodistplot <- renderPlot({
  if(!is.network(nwreac())){
    return()
  }
  g <- geodist(nwreac(),inf.replace=0)
  barplot(sort(c(g$gdist), decreasing=TRUE),  col="#3182bd", border=NA,
          xlab = "Vertex Pairs", ylab = "Shortest Path")
  if(input$uniformoverlay_gd){
    lines(gd_uniformoverlay(), lwd=2,
            col=adjustcolor('firebrick4', alpha.f = 1))
  }
  if(input$bernoullioverlay_gd){
    lines(gd_bernoullioverlay(), lwd=2,
          col=adjustcolor('orangered', alpha.f = 1))
  }
})

output$geodistdownload <- downloadHandler(
  filename = function(){paste(nwname(),'_geodist.pdf',sep='')},
  content = function(file){
    pdf(file=file, height=10, width=15)
    g <- geodist(nwreac(),inf.replace=0)
    barplot(sort(c(g$gdist), decreasing=TRUE), col="#3182bd", border=NA,
            xlab = "Vertex Pairs", ylab = "Shortest Path")
    if(input$uniformoverlay_gd){
      lines(c(gd_uniformoverlay()), lwd=2,
             col=adjustcolor('firebrick4', alpha.f = 1))
    }
    if(input$bernoullioverlay_gd){
      lines(c(gd_bernoullioverlay()), lwd=2,
             col=adjustcolor('orangered', alpha.f = 1))
    }
    dev.off()
  })

#MORE
#since the visibility toggles between two states, set the options to 
#not suspend the output when hidden
output$mixmxchooser <- renderUI({
  selectInput('mixmx', label='Choose attribute',
              choices = menuattr(), selectize = FALSE)
})
outputOptions(output,'mixmxchooser',suspendWhenHidden=FALSE)

output$mixingmatrix <- renderPrint({
  if(!is.network(nwreac())) {return()}
  if(!is.null(input$mixmx)){
  mixingmatrix(nwreac(), input$mixmx)}
})
outputOptions(output,'mixingmatrix',suspendWhenHidden=FALSE)

output$gden <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  gden(nwreac(), diag=has.loops(nwreac()), mode=gmode)
})
outputOptions(output,'gden',suspendWhenHidden=FALSE)

output$grecip <- renderText({
  if(!is.network(nwreac())) {return()}
  if(input$grecipmeas == ''){
    return('Reciprocity:')
  }
  grecip(nwreac(), measure=input$grecipmeas)
})
outputOptions(output,'grecip',suspendWhenHidden=FALSE)

output$gtrans <- renderText({
  if(!is.network(nwreac())) {return()}
  if(input$gtransmeas == ''){
    return('Transitivity:')
  }
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  gtrans(nwreac(), diag=has.loops(nwreac()), mode=gmode,
         measure=input$gtransmeas)
})
outputOptions(output,'gtrans',suspendWhenHidden=FALSE)

output$ndeg <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  d <- degree(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
         cmode=input$ndegcmode)
})
outputOptions(output,'ndeg',suspendWhenHidden=FALSE)

output$nbetw <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  b <- betweenness(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
                   cmode=input$nbetwcmode)
})
outputOptions(output,'nbetw',suspendWhenHidden=FALSE)

output$nclose <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  c <- closeness(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
                 cmode=input$nclosecmode)
})
outputOptions(output,'nclose',suspendWhenHidden=FALSE)

output$nstress <- renderText({
  if(!is.network(nwreac())){ return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  s <- stresscent(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
                  cmode=input$nstresscmode)
})
outputOptions(output,'nstress',suspendWhenHidden=FALSE)

output$ngraphcent <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  g <- graphcent(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
                  cmode=input$ngraphcentcmode)
})
outputOptions(output,'ngraphcent',suspendWhenHidden=FALSE)

output$nevcent <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  e <- evcent(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()))
})
outputOptions(output,'nevcent',suspendWhenHidden=FALSE)

output$ninfocent <- renderText({
  if(!is.network(nwreac())) {return()}
  if(is.directed(nwreac())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  i <- infocent(nwreac(), nodes=input$nodeind, gmode=gmode, diag=has.loops(nwreac()),
                 cmode=input$ninfocentcmode)
})
outputOptions(output,'ninfocent',suspendWhenHidden=FALSE)

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
  if(!is.network(nwreac())){
    return()
  }
  if(input$matchingorall == 'All terms'){
    current.terms <- unlist(allterms)
  } else {
    matchterms <- search.ergmTerms(net=nwreac())
    ind <- gregexpr(pattern='\\(', matchterms)
    for(i in 1:length(matchterms)){
      matchterms[i] <- substr(matchterms[[i]], start=1, stop=ind[[i]][1]-1)
    }
    matchterms <- unique(matchterms)
    current.terms <- unlist(matchterms)
  }
  selectInput('termdoc',label = NULL,
                  choices = current.terms,
                  multiple=FALSE, 
                  selectize=FALSE)
  
})

output$termdoc <- renderPrint({
  myterm <- input$termdoc
  search.ergmTerms(name=myterm)
})


#' Below I output the current formulation of the ergm 
#' model so the user can clearly see how their menu selections change the model.
#' Since `ergm.terms()` is a reactive object, it will automatically update when
#' the user clicks on menu options.
#'  
#+ fitmodel2, eval=FALSE
output$currentdataset1 <- renderPrint({
  if(!is.network(nwreac())){
    return(cat('Upload a network'))
  }
  cat(isolate(nwname()))
})

output$checkterms1 <- renderPrint({
  if(!is.network(nwreac())){
    return(cat('Upload a network'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
  cat(ergm.terms())
})

output$prefitsum <- renderPrint({
  if(!is.network(nwreac()) | length(input$terms)==0){
    return(cat('NA'))
  }
  if(ergm.terms()=='NA') return(cat('Add terms to the formula'))
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

#make sure that mcmc iterations output on the fitting tab by allowing
#modelfit to update even when other tab is active

#other potential methods were to use onFlush, or to set the priority of
#observers, but this is the best and least complicated (and the only 
#one that works)

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
  if(!is.network(nwreac())){
    return(cat('Upload a network'))
  }
  cat(nwname())
})

#formula only updates after fitButton has been clicked
output$checkterms2 <- renderPrint({
  if(!is.network(nwreac())){
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

#' **Diagnostics - MCMC Diagnostics**
#' 
#' When using the `mcmc.diagnostics` function in the command line, the printed 
#' diagnostics and plots all output together. Instead of calling `mcmc.diagnositcs`
#' a reactive object, .
#' 
#+ eval=FALSE

output$checkterms3 <- renderPrint({
  if(is.null(nwreac())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  cat(isolate(ergm.terms()))
})
output$currentdataset3 <- renderPrint({
  if(!is.network(nwreac())){
    return(cat('Upload a network'))
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
  if(!is.network(nwreac())){
    return(cat('Upload a network'))
  }
  if(input$fitButton == 0){
    return(cat('Please fit a model'))
  }
  cat(isolate(ergm.terms()))
})
output$currentdataset4 <- renderPrint({
  if(!is.network(nwreac())){
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

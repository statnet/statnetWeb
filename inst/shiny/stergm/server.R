
## server.R for stergm app

library(statnet)
library(tergm)
library(xlsx)
library(RColorBrewer)

data(faux.mesa.high)
data(florentine)
data(sampson)
data(samplk)
data(ecoli)
data(molecule)
data(kapferer)
sampson <- list()
sampson[[1]] <- samplk1
sampson[[2]] <- samplk2
sampson[[3]] <- samplk3


shinyServer(
  function(input, output, session){

# Reactive Values ---------------------------------------------------------
## Roughly in order of use


values <- reactiveValues()

# when two options are available to the user, or when we need to know if one
# variable is outdated this reactive value will keep track of the state
# state <- reactiveValues(symmdir = FALSE, plotperc_dd = FALSE,
#                         plotperc_gd = FALSE, allterms = FALSE, gof = 0)
#
# # To keep a list of all attributes uploaded by the user:
# values$v_attrNamesToAdd <- list(1)
# values$v_attrValsToAdd <- list()
# values$e_attrNamesToAdd <- list(1)
# values$e_attrValsToAdd <- list()
# values$ev_attrNamesToAdd <- list(1)
# values$ev_attrValsToAdd <- list()
# values$input_termslist <- list()

values$nwnum <- "single"


#     #move to Help page when user clicks Help link button
#     observe({
#       if(input$helpLink == 0) {return()}
#       isolate({
#         updateTabsetPanel(session, 'navbar', selected = 'tab8')
#       })
#     })

#move to Data panel when user clicks Get Started button
observe({
  if(input$startButton == 0) {return()}
  isolate({
    updateTabsetPanel(session, 'navbar', selected = 'tab2')
  })
})

#update active tab in navbar when arrows are clicked
# leftarrowclicks <- reactive({
#   input$dataleft+input$plotleft+input$fitleft
# })
# rightarrowclicks <- reactive({
#   input$dataright+input$plotright+input$fitright
# })
# observe({
#   if(leftarrowclicks() == 0) {return()}
#   tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
#   current <- isolate(which(input$navbar == tabOptions))
#   updateTabsetPanel(session, 'navbar', selected = tabOptions[current-1])
# })
# observe({
#   if(rightarrowclicks() == 0) {return()}
#   tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
#   current <- isolate(which(input$navbar == tabOptions))
#   updateTabsetPanel(session, 'navbar', selected = tabOptions[current+1])
# })

observeEvent(input$nwnum1,{
  values$nwnum <- "single"
})
observeEvent(input$nwnum2,{
  values$nwnum <- "multiple"
})

#nwinit is used to get the initial values of the network
nwinit <- reactive({
  #input$rawdatafile comes as a dataframe with name, size, type and datapath
  #datapath is stored in 4th column of dataframe
  input$uploadnet
  nw_var <- ""
isolate({
  if(!is.null(input$rawdatafile)){
    filepath <- input$rawdatafile[1,4]
    filename <- input$rawdatafile[1,1]
    fileext <- substr(filename,
                      max(gregexpr(pattern = ".",
                                   text = filename, fixed = TRUE)[[1]]),
                      nchar(filename))

    if(input$filetype == "statnet"){
      validate(
        need(fileext %in% c(".rds", ".Rds", ".RDs", ".RDS"),
             "Upload an .rds file"))
      nw_var <- readRDS(paste(filepath))
    } else if(input$filetype == "rmat"){
      validate(
        need(fileext %in% c(".rds", ".Rds", ".RDs", ".RDS"),
             "Upload the specified type of matrix"))
      newmx <- readRDS(paste(filepath))
      if(values$nwnum == "multiple"){
        nw_var <- lapply(newmx, FUN = function(x){
          network(x,
                  directed = input$dir,
                  loops = input$loops,
                  multiple = input$multiple,
                  bipartite = input$bipartite,
                  matrix.type = input$matrixtype,
                  ignore.eval = FALSE,
                  names.eval = 'edgevalue')
        })
      } else {
        nw_var <- network(newmx,
                          directed = input$dir,
                          loops = input$loops,
                          multiple = input$multiple,
                          bipartite = input$bipartite,
                          matrix.type = input$matrixtype,
                          ignore.eval = FALSE,
                          names.eval = 'edgevalue')
      }
    } else if(input$filetype == "excel"){
      validate(
        need(fileext %in% c(".xlsx"),
             "Upload the specified type of matrix"))
      header <- TRUE
      row_names <- 1
      if(input$matrixtype == "edgelist"){
        header <- FALSE
        row_names<-NULL
      }
      if(values$nwnum == "single"){
        try({nw_var <- network(read.xlsx(paste(filepath),
                                         sheetIndex = 1,
                                         as.data.frame = TRUE,
                                         header = header,
                                         row.names = row_names),
                               directed = input$dir,
                               loops = input$loops,
                               multiple = input$multiple,
                               bipartite = input$bipartite,
                               matrix.type = input$matrixtype,
                               ignore.eval = FALSE,
                               names.eval = 'edgevalue')
        })
      } else if(values$nwnum == "multiple"){
        nw_var <- lapply(1:input$npanels, FUN = function(x){
          network(
            read.xlsx(paste(filepath),
                      sheetIndex = x,
                      as.data.frame = TRUE,
                      header = header,
                      row.names = row_names),
            directed = input$dir,
            loops = input$loops,
            multiple = input$multiple,
            bipartite = input$bipartite,
            matrix.type = input$matrixtype,
            ignore.eval = FALSE,
            names.eval = 'edgevalue')
        })
      }
    } else if(input$filetype == "pajeknet"){
      validate(
        need(fileext %in% c(".net", ".NET"),
             "Upload a .net file"))
      nw_var <- read.paj(paste(filepath))
    } else if(input$filetype == "pajekpaj"){
      validate(
        need(fileext %in% c(".paj",".PAJ"),
             "Upload a .paj file"))
      nws <- read.paj(paste(filepath))
      if(!is.null(pajnws())){
        nw_var <- nws$networks[[as.numeric(input$choosepajnw)]]
      }
    }
    if(values$nwnum == "multiple" & "network" %in% class(nw_var)){
      nw_var <- networkDynamic(base.net = nw_var[[1]],
                               network.list = nw_var,
                               onsets = seq(from = 0, length = length(nw_var)),
                               termini = seq(from = 1, length = length(nw_var)),
                               verbose = FALSE,
                               create.TEAs = FALSE) # change to TRUE if attributes change through time
    }
  }

  })
  if(input$filetype == "builtin"){
    if(!is.null(input$samplenet)){
      if(input$samplenet == ""){
        nw_var <- ""
      } else {
        nw_var <- eval(parse(text = input$samplenet))
        if("network" %in% class(nw_var)){
          if(!is.element('bipartite', names(nw_var$gal))){
            set.network.attribute(nw_var, 'bipartite', FALSE)
          }
        }
        if(values$nwnum == "multiple"){
          nw_var <- networkDynamic(base.net = nw_var[[1]],
                                   network.list = nw_var,
                                   onsets = seq(from = 0, length = length(nw_var)),
                                   termini = seq(from = 1, length = length(nw_var)),
                                   verbose = FALSE,
                                   create.TEAs = FALSE) # change to TRUE if attributes change through time
        }
      }

    }
  }

  return(nw_var)
})

#list of everything in an uploaded Pajek project
pajnws <- reactive({
  nws <- NULL
  if((input$filetype == "pajekpaj") & (!is.null(input$rawdatafile))){
    filename <- input$rawdatafile[1,1]
    if(substr(filename,nchar(filename)-3,nchar(filename))==".paj"){
      nws <- read.paj(paste(input$rawdatafile[1,4]))
    }
  }
  nws
})

nwname <- reactive({
  name <- input$rawdatafile[1,1]
  if(input$filetype == "builtin"){
    name <- input$samplenet
  }
  name
})

#number of nodes in nw
nodes <- reactive({
  if(!is.network(nwinit())){return()}
  nwinit()$gal$n
})

# #number of edges in initial nw
# nedgesinit <- reactive({
#   if (is.network(nwinit())){
#     network.edgecount(nwinit())
#   } else if (class(nwinit()) == "list"){
#     network.edgecount(nwinit()[[1]])
#   }
# })
#
# #initial vertex attributes
# vattrinit <- reactive({
#   vattrinit <- c()
#   if(is.network(nwinit())){
#     vattrinit<-list.vertex.attributes(nwinit())
#   }
#   vattrinit
# })
#
# #matrix of vertex attribute values
# vattrinit.vals <- reactive({
#   v <- list()
#   for (j in seq(length(list.vertex.attributes(nwinit())))) {
#     v[[j]] <- get.vertex.attribute(nwinit(), vattrinit()[j])
#   }
#   v
# })

#set correct number of rows for the attribute value lists,
#so that we can add columns later
# observe({
#   nwinit()
#   #reset lists when uploaded network changes
#   vdf <- list()
#   edf <- list()
#   evdf <- list()
#   if (is.network(nwinit())){
#     n <- nodes()
#     e <- nedgesinit()
#     for (i in 1:n){
#       vdf <- rbind(vdf,i)
#     }
#     for (i in 1:e){
#       edf <- rbind(edf,i)
#       evdf <- rbind(evdf,i)
#     }
#     values$v_attrValsToAdd <- vdf
#     values$e_attrValsToAdd <- edf
#     values$ev_attrValsToAdd <- evdf
#     values$v_attrNamesToAdd <- list(1)
#     values$e_attrNamesToAdd <- list(1)
#     values$ev_attrNamesToAdd <- list(1)
#
#     values$vertexnames <- network.vertex.names(nwinit())
#   }
# })

#names of uploaded attributes
#or helpful message that upload is incorrect
newattrnamereac <- reactive({
  newname <- ''
  try({
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]
    fileext <- substr(filename,nchar(filename)-3,nchar(filename))

    if(fileext %in% c(".csv", ".CSV") ){
      newattrs <- read.csv(paste(path), sep=",", header=TRUE,
                           stringsAsFactors=FALSE)
      newname <- names(newattrs)
      if(input$newattrtype == "edgeattr" & input$edgeform == "matrix"){
        newname <- substr(filename, 1, nchar(filename)-4)
      }
    } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS") ){
      newattrs <- readRDS(paste(path))
      newname <- names(newattrs)
      if(class(newattrs) != "list"){
        newname <- "Attribute is not compatible, see help buttons and try again"
      }
    } else {
      newname <- "Attribute is not compatible, see help buttons and try again"
    }

  })
  if(is.null(newname)){
    newname <- "Attribute is not named,  please fix and re-upload"
  }
  newname
})

# #save new vertex names
# observeEvent(input$newattrButton, {
#   if(input$newattrtype == "vertexnames"){
#     path <- input$newattrvalue[1,4]
#     filename <- input$newattrvalue[1,1]
#     fileext <- substr(filename,nchar(filename)-3,nchar(filename))
#     if(fileext %in% c(".csv", ".CSV")){
#       newnames <- read.csv(paste(path), sep=",", header=TRUE,
#                            stringsAsFactors=FALSE)
#       newnames <- newnames[[1]]
#     } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
#       newnames <- readRDS(paste(path))
#     }
#
#     values$vertexnames <- newnames
#   }
# })
#
# #add vertex attributes to list
# observeEvent(input$newattrButton, {
#   if(input$newattrtype == "vertexattr"){
#     path <- input$newattrvalue[1,4]
#     filename <- input$newattrvalue[1,1]
#     fileext <- substr(filename,nchar(filename)-3,nchar(filename))
#     if(fileext %in% c(".csv", ".CSV")){
#       newattrs <- read.csv(paste(path), sep=",", header=TRUE,
#                            stringsAsFactors=FALSE)
#       newname <- names(newattrs)
#     } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
#       newattrs <- readRDS(paste(path))
#       newname <- names(newattrs)
#     }
#
#     namesofar <- values$v_attrNamesToAdd
#     valsofar <- values$v_attrValsToAdd
#     for(k in 1:length(newname)){
#       namesofar <- cbind(namesofar, newname[[k]])
#       valsofar <- cbind(valsofar, newattrs[[k]])
#     }
#
#     values$v_attrNamesToAdd <- namesofar
#     values$v_attrValsToAdd <- valsofar
#   }
# })
#
# #add edge attributes to list
# observeEvent(input$newattrButton, {
#   if(input$newattrtype == "edgeattr" & input$edgeform == "vector"){
#     path <- input$newattrvalue[1,4]
#     filename <- input$newattrvalue[1,1]
#     fileext <- substr(filename,nchar(filename)-3,nchar(filename))
#     if(fileext %in% c(".csv", ".CSV")){
#       newattrs <- read.csv(paste(path), sep=",", header=TRUE,
#                            stringsAsFactors=FALSE)
#       newname <- names(newattrs)
#     } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
#       newattrs <- readRDS(paste(path))
#       newname <- names(newattrs)
#     }
#
#     namesofar <- values$e_attrNamesToAdd
#     valsofar <- values$e_attrValsToAdd
#     for(k in 1:length(newname)){
#       namesofar <- cbind(namesofar, newname[[k]])
#       valsofar <- cbind(valsofar, newattrs[[k]])
#     }
#     values$e_attrNamesToAdd <- namesofar
#     values$e_attrValsToAdd <- valsofar
#   }
# })
#
# #add edge values to list
# observeEvent(input$newattrButton, {
#   if(input$newattrtype == "edgeattr" & input$edgeform == "matrix"){
#     path <- input$newattrvalue[1,4]
#     filename <- input$newattrvalue[1,1]
#     fileext <- substr(filename,nchar(filename)-3, nchar(filename))
#     if(fileext %in% c(".csv", ".CSV")){
#       newattrs <- read.csv(paste(path), sep=",", header=TRUE,
#                            row.names = 1,
#                            stringsAsFactors=FALSE)
#       newname <- substr(filename, 1, nchar(filename)-4)
#       newattrs <- data.matrix(newattrs, rownames.force=FALSE)
#     } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
#       newattrs <- readRDS(paste(path))
#       newname <- names(newattrs)
#     }
#     namesofar <- values$ev_attrNamesToAdd
#     valsofar <- values$ev_attrValsToAdd
#     j <- length(valsofar)
#     for(k in 1:length(newname)){
#       namesofar <- cbind(namesofar, newname[[k]])
#       valsofar[[j+k]] <- newattrs[[k]]
#     }
#     values$ev_attrNamesToAdd <- namesofar
#     values$ev_attrValsToAdd <- valsofar
#   }
# })
#
# observeEvent(input$symmdir,{
#   state$symmdir <- TRUE
# })
# observeEvent(input$symmundir,{
#   state$symmdir <- FALSE
# })

#attributes will be added to this network
# nwmid <- reactive({
#   nw_var <- nwinit()
#
#   if (class(nw_var)=="network"){
#     #preserve initial network attributes and let user choose if directed
#     #after symmetrizing
#     if(input$symmetrize != "Do not symmetrize"){
#       symnw <- sna::symmetrize(nw_var, rule=input$symmetrize)
#       nw_var <- network(symnw, matrix.type="adjacency", directed=state$symmdir,
#                         hyper=nwattrinit()[2], loops=nwattrinit()[3],
#                         multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
#       #add initial vertex attributes back after symmetrizing
#       #can't add edge attributes back because number of edges has changed
#       for(k in 1:length(vattrinit())){
#         attr_names <- vattrinit()
#         attr_list <- vattrinit.vals()
#         set.vertex.attribute(nw_var, attr_names[k], attr_list[[k]])
#       }
#     }
#
#     if (is.bipartite(nw_var)){
#       set.vertex.attribute(nw_var, "mode", c(rep(1, nw_var$gal$bipartite),
#                                              rep(2, nw_var$gal$n - nw_var$gal$bipartite)))
#     }
#
#     v_attrNamesToAdd <- values$v_attrNamesToAdd
#     v_attrValsToAdd <- values$v_attrValsToAdd
#     e_attrNamesToAdd <- values$e_attrNamesToAdd
#     e_attrValsToAdd <- values$e_attrValsToAdd
#     ev_attrNamesToAdd <- values$ev_attrNamesToAdd
#     ev_attrValsToAdd <- values$ev_attrValsToAdd
#
#
#     if(input$newattrButton > 0){
#       try({network.vertex.names(nw_var) <- values$vertexnames})
#     }
#     v_numnew <- length(v_attrNamesToAdd)
#     if(v_numnew > 1){
#       for (j in 2:v_numnew){
#         try({v_newname <- as.character(v_attrNamesToAdd[1,j])
#         v_newval <- v_attrValsToAdd[,j]
#         set.vertex.attribute(nw_var,v_newname,v_newval)})
#       }
#     }
#
#     e_numnew <- length(e_attrNamesToAdd)
#     if(e_numnew > 1){
#       for (k in 2:e_numnew){
#         try({e_newname <- as.character(e_attrNamesToAdd[1,k])
#         e_newval <- e_attrValsToAdd[,k]
#         set.edge.attribute(nw_var,e_newname,e_newval)})
#       }
#     }
#
#     ev_numnew <- length(ev_attrNamesToAdd)
#     if(ev_numnew > 1){
#       for (l in 2:ev_numnew){
#         try({ev_newname <- as.character(ev_attrNamesToAdd[1,l])
#         ev_newval <- ev_attrValsToAdd[[l]]
#         set.edge.value(nw_var,ev_newname,ev_newval)})
#       }
#     }
#   }
#
#   nw_var
# })

#use this network for future calculations
nw <- reactive({
  nw_var <- nwinit()

  values$input_termslist <- list()

  nw_var
})

# elist <- reactive({
#   if(!is.network(nwinit())) return()
#   as.edgelist(nw())
# })
#
# #get coordinates to plot network with
coords <- reactive({
  input$refreshplot
  plot.network(nw())
})

#initial network attributes
#returns vector of true/falses
nwattrinit <- reactive({
  if(!is.network(nwinit())){return()}
  nwattributes <- c('directed', 'hyper', 'loops', 'multiple', 'bipartite')
  unlist(lapply(nwattributes, get.network.attribute, x = nwinit()))
})

#list of all vertex attributes in nw (after adding new)
attrib <- reactive({
  attr <- c()
  if(is.network(nw())){
    attr <- list.vertex.attributes(nw())
  }
  attr
})

#don't allow "na" or "vertex.names" as vertex attributes in menus on fit tab
menuattr <- reactive({
  menuattr <- attrib()
  if(is.element("na", menuattr)){
    menuattr <- menuattr[-which("na" == menuattr)]
  }
  if(is.element("vertex.names", menuattr)){
    menuattr <- menuattr[-which("vertex.names" == menuattr)]
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
  numattr
})
#
# #dataframe of nodes, their attributes, and their coordinates in nwplot
# nwdf <- reactive({
#   attrs <- menuattr()
#   if(is.na(as.numeric(network.vertex.names(nw()))[1])){
#     df <- data.frame(Names = network.vertex.names(nw()))
#   } else {
#     df <- data.frame(Names = as.numeric(network.vertex.names(nw())))
#   }
#   for(i in seq(length(attrs))){
#     df[[attrs[i]]] <- get.vertex.attribute(nw(), attrs[i])
#   }
#   df[["Missing"]] <- get.vertex.attribute(nw(), "na")
#   df[["cx"]] <- coords()[,1]
#   df[["cy"]] <- coords()[,2]
#   df
# })

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
  sna::betweenness(nw(), gmode = gmode, diag = has.loops(nw()),
                   cmode = cmode)
})

nodesize <- reactive({
  if(!("network" %in% class(nw()))){return()}
  nw_var <- nw()
  #scale size of nodes onto range between .7 and 3.5
  if (input$sizeby == '1'){
    size = 1
  } else if (input$sizeby == 'Betweenness'){
    minsize <- min(nodebetw())
    maxsize <- max(nodebetw())
    size = (nodebetw()-minsize)/(maxsize-minsize)*(3.5-.7)+.7
  } else {
    minsize <- min(get.vertex.attribute(nw_var,input$sizeby))
    maxsize <- max(get.vertex.attribute(nw_var,input$sizeby))
    size <- (get.vertex.attribute(nw_var,input$sizeby)-minsize) /
      (maxsize-minsize) * (3.5 - .7) + .7
  }
  size})

#vertex color
vcol <- reactive({
  if(!is.network(nw())){return()}
  nw_var <- nw()
  if(input$colorby == 2){
    vcol <- rep(2, nodes())
  } else {
    full_list <- get.vertex.attribute(nw_var,input$colorby)
    short_list <- sort(unique(full_list))
    ncolors <- length(short_list)
    if(is.element("Other", short_list)){ #to be consistent with order of legend
      short_list <- short_list[-which(short_list == "Other")]
      short_list <- c(short_list, "Other")
    }
    full_list <- match(full_list, short_list)
    #each elt corresponds to integer position in short_list
    pal <- c('red', 'blue', 'green3', 'cyan', 'magenta3',
             'yellow', 'orange', 'black', 'grey')
    if(ncolors>9){
      pal <- colorRampPalette(brewer.pal(11,"RdYlBu"))(ncolors)
    }
    vcol <- pal[full_list]
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
      legendlabels <- legendlabels[-which(legendlabels == "Other")]
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
    if(n>9){
      pal <- colorRampPalette(brewer.pal(11,"RdYlBu"))(n)
    }
    legendfill <- adjustcolor(pal, alpha.f = input$transp)
  }
  legendfill
})

#simulated graphs for cug tests
# observeEvent(c(nw(), input$ncugsims),{
#   if(!is.null(nw())){
#     s <- network.edgecount(nw())
#     if (is.directed(nw())){
#       mode <- "digraph"
#     } else {
#       mode <- "graph"
#     }
#
#     brgsims <- sna::rgraph(n = nodes(), m = input$ncugsims,
#                            tprob = sna::gden(nw()), mode = mode,
#                            diag = nw()$gal$loops)
#     cugsims <- sna::rgnm(n = input$ncugsims, nv = nodes(),
#                          m = s, mode = mode,
#                          diag = nw()$gal$loops)
#
#     values$cugsims <- list(brgsims, cugsims)
#   }
# })

# brgvals <- reactive({
#   apply(values$cugsims[[1]], MARGIN = 1, FUN = cugstats,
#         term = input$cugtestterm, directed = nw()$gal$directed,
#         loops = nw()$gal$loops)
# })
# cugvals <- reactive({
#   apply(values$cugsims[[2]], MARGIN = 1, FUN = cugstats,
#         term = input$cugtestterm, directed = nw()$gal$directed,
#         loops = nw()$gal$loops)
# })

## FIT MODEL ##
#
# formation <- reactive({
#   input$updateformulaButton
#   isolate({input$formation})
# })
#
# dissolution <- reactive({
#   paste(input$dissolution, collapse = " + ")
# })
#
# observeEvent(input$resetformulaButton, {
#   updateTextInput(session, "formation",
#                   label = NULL, value = "edges")
# })
# observeEvent(nw(), {
#   updateTextInput(session, "formation",
#                   label = NULL, value = "edges")
# })
#
# dissoffsets <- reactive({
#   ncoefs <- length(input$dissolution)
#   ids <- paste0("coef", ncoefs)
#   unlist(input[ids])
# })
#
# estimate <- reactive({
#   if("networkDynamic" %in% class(nw())){
#     "CMLE"
#   } else {
#     "EGMME"
#   }
# })
#
# stergmcontrols <- reactive({
#   customcontrols <- isolate(paste(input$customMCMCcontrol, sep = ","))
#   if(customcontrols == ""){
#     if(estimate() == "EGMME"){
#       control.stergm(EGMME.MCMC.burnin.min = input$EGMME.burnin.min,
#                      EGMME.MCMC.burnin.max = input$EGMME.burnin.max,
#                      EGMME.MCMC.burnin.add = input$EGMME.burnin.add,
#                      EGMME.MCMC.burnin.pval = input$EGMME.burnin.pval)
#     }
#   } else {
#     if(estimate() == "EGMME"){
#       control.stergm(EGMME.MCMC.burnin.min = input$EGMME.burnin.min,
#                      EGMME.MCMC.burnin.max = input$EGMME.burnin.max,
#                      EGMME.MCMC.burnin.add = input$EGMME.burnin.add,
#                      EGMME.MCMC.burnin.pval = input$EGMME.burnin.pval,
#                      eval(parse(text = customcontrols)))
#     }
#   }
# })
#
# #stergm model object
# stergm.fit <- reactive({
#   if(input$fitButton == 0){
#     return()
#   }
#   usingdefault <- isolate(input$controldefault)
#   if(usingdefault){
#     isolate({
#       fit <- stergm(nw(),
#                     formation = as.formula(paste("~", formation())),
#                     dissolution = as.formula(paste("~", dissolution())),
#                     targets = "formation",
#                     offset.coef.diss = dissoffsets(),
#                     estimate = estimate())
#     })
#   } else {
#       fit <- stergm(nw(),
#                     formation = as.formula(paste("~", formation())),
#                     dissolution = as.formula(paste("~", dissolution())),
#                     targets = "formation",
#                     offset.coef.diss = dissoffsets(),
#                     estimate = estimate(),
#                     control = stergmcontrols())
#   }
#
#   return(fit)
# })






# Output Objects ----------------------------------------------------------

output$rawdatafile <- renderPrint({
  raw <- matrix(nrow = 2, ncol = 1)
  rownames(raw)<-c("name:", "size:")
  if(!is.null(input$rawdatafile)){
    raw[1, 1] <- input$rawdatafile[1, 1]
    raw[2, 1] <- paste(input$rawdatafile[1, 2], " bytes")
  }
  write.table(raw, quote = FALSE, col.names = FALSE)
})

output$samplenetUI <- renderUI({
  if(values$nwnum == "single"){
    nws <- c("Choose a network" = "",
             "ecoli1", "ecoli2",
             "faux.mesa.high", "flobusiness",
             "flomarriage", "kapferer",
             "kapferer2", "molecule",
             "samplike", "samplk1",
             "samplk2", "samplk3")
  } else if(values$nwnum == "multiple"){
    nws <- c("Choose a network" = "",
             "sampson")
  }
  selectizeInput('samplenet', label = NULL,
                 choices = nws)
})
outputOptions(output, "samplenetUI", suspendWhenHidden = FALSE, priority = 50)

output$npanelsui <- renderUI({
  if(values$nwnum == "multiple" & input$filetype == "excel"){
    numericInput("npanels", label = "Number of network panels",
                 value = 2, min = 2, step = 1)
  }
})

output$pajchooser <- renderUI({
  pajlist <- c(None = '')
  if(!is.null(pajnws())){
    pajlist <- 1:length(pajnws()$networks)
    names(pajlist) <- names(pajnws()$networks)
  }
  selectInput('choosepajnw',
              label = 'Choose a network from the Pajek project',
              choices = pajlist)
})
outputOptions(output, "pajchooser", suspendWhenHidden = FALSE)


output$newattrname <- renderPrint({
  if(!is.null(input$newattrvalue)){
    cat(newattrnamereac())}
})

#summary of network attributes
output$nwsum <- renderPrint({
  if (!("network" %in% class(nw()))){
    return(cat('NA'))
  }
  nw_var <- nw()
  return(nw_var)
})

## DESCRIPTIVES ##

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
              c('None' = 2, attrib()))
})
outputOptions(output,'dynamiccolor', suspendWhenHidden=FALSE, priority=10)

# need this to know when color palette will change
output$attrlevels <- renderText({
  return(length(legendlabels()))
})
outputOptions(output,'attrlevels', suspendWhenHidden=FALSE, priority=10)

output$dynamicsize <- renderUI({
  choices <- c('None' = 1, numattr())
  if(values$nwnum == "single"){
    choices <- append(choices, "Betweenness")
  }
  selectInput('sizeby',
              label = 'Size nodes according to:',
              choices = choices)
})
outputOptions(output,'dynamicsize',suspendWhenHidden=FALSE)

output$nwplot <- ndtv:::renderNdtvAnimationWidget({
  if (!("network" %in% class(nw()))){
    return()
  }
  input$plottabs
  input$rawdatafile
  input$samplenet

  color <- adjustcolor(vcol(), alpha.f = input$transp)
  vcex <- nodesize()
  if(is.bipartite(nw())){
    sides <- c(rep(50, nw()$gal$bipartite),
               rep(3, nodes() - nw()$gal$bipartite))
  } else{
    sides <- 50
  }
  render.d3movie(nw(),
                 output.mode = "htmlWidget",
                 launchBrowser = FALSE,
                 displaylabels = input$vnames,
                 vertex.col = color,
                 vertex.sides = sides,
                 vertex.cex = vcex,
                 coords = coords())
})

output$legendplot <- renderPlot({
  par(mar = c(0, 0, 0, 0))
  plot.new()
  if(input$colorby != 2){
    legend("topleft",
           title=input$colorby,
           legend = legendlabels(),
           fill = legendfill())
  }
})

## FIT MODEL ##

observeEvent(input$matchingButton, {
  state$allterms <- FALSE
})
observeEvent(input$allButton, {
  state$allterms <- TRUE
})

output$listofterms <- renderUI({
  if(!is.network(nw())){
    return()
  }
  if(state$allterms){
    current.terms <- allterms$names
  } else {
    matchterms <- splitargs(nw = nw())
    current.terms <- matchterms$names
  }
  selectizeInput('chooseterm', label = NULL,
                 choices = c("Select a term" = "", current.terms))
})

output$termdoc <- renderUI({
  myterm <- input$chooseterm
  if(is.null(myterm)){
    return(p("Select or search for a term in the menu above."))
  } else if(myterm == ""){
    return(p("Select or search for a term in the menu above."))
  }
  chrvec <- capture.output(search.ergmTerms(name = myterm))
  desc <- strsplit(chrvec[3], split = "_")
  p(chrvec[1], br(),br(),
    strong(chrvec[2]), br(),br(),
    em(desc[[1]][2]), desc[[1]][3], br(),
    chrvec[4])
})

output$currentnw1 <- renderPrint({
  if(!is.network(nw())){
    return(cat('Upload a network'))
  }
  cat(isolate(nwname()))
})

output$form <- renderPrint({
  cat(paste("~", formation()))
})

output$diss <- renderPrint({
  cat(paste("~", dissolution()))
})

output$dissterms <- renderUI({
  terms <- trimws(strsplit(formation(), "+", fixed = TRUE)[[1]])
  offterms <- paste0("offset(", terms, ")")
  div(
    selectInput("dissolution", label = NULL,
                choices = offterms, multiple = TRUE)
  )
})

output$disscoefs <- renderUI({
  ncoefs <- length(input$dissolution)
  if(ncoefs == 0) {return()}
  ids <- paste0("coef", seq(ncoefs))
  lapply(ids, FUN = numericInput, value = 1, label = NULL)
})

output$prefitsum <- renderPrint({
  if(!is.network(nw()) | length(input$formation) == 0){
    return(cat('NA'))
  }
  options(width = 140)
  summary(as.formula(paste("nw() ~", formation())))
})

output$modelfit <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After specifiying stergm model, click "Fit Model" above.'))
  }
  options(width = 140)
  stergm.fit()
})
outputOptions(output, "modelfit", priority = 10)

output$modelfitsum <- renderPrint({
  if (input$fitButton == 0){
    return(cat('After specifiying stergm model, click "Fit Model" above.'))
  }
  options(width = 140)
  summary(stergm.fit())
})

})

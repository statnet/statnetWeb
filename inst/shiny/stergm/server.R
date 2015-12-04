
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
data(windsurfers)
sampson <- list()
sampson[[1]] <- samplk1
sampson[[2]] <- samplk2
sampson[[3]] <- samplk3


BRGcol <- "darkred"
CUGcol <- "darkorange"
obsblue <- "#076EC3"
histblue <- "#83B6E1"
tgray3 <- adjustcolor("gray", alpha.f = 0.3)
tgray7 <- adjustcolor("gray", alpha.f = 0.7)

shinyServer(
  function(input, output, session){

# Reactive Values ---------------------------------------------------------
## Roughly in order of use


values <- reactiveValues()

# when two options are available to the user, or when we need to know if one
# variable is outdated this reactive value will keep track of the state
state <- reactiveValues(symmdir = FALSE, plotperc_dd = FALSE,
                        plotperc_gd = FALSE, allterms = FALSE, gof = 0)

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
  validate(
    need(!(input$filetype %in% c("pajeknet", "pajekpaj") & values$nwnum == "multiple"),
         "Cannot accept multiple network panels from Pajek at this time"))
  nw_var <- ""
isolate({
  if(input$filetype != "builtin" & !is.null(input$rawdatafile)){
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
      # could be single network or list of networks or networkDynamic obj
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
    if(values$nwnum == "multiple" & !("networkDynamic" %in% class(nw_var))){
      validate(need(!is.network(nw_var),
                    message = "Please upload a list of networks or a networkDynamic object"))
      # if uploaded file is a list of networks, create nD
      nw_var <- networkDynamic::networkDynamic(base.net = nw_var[[1]],
                               network.list = nw_var,
                               onsets = seq(from = 0, length = length(nw_var)),
                               termini = seq(from = 1, length = length(nw_var)),
                               verbose = FALSE,
                               create.TEAs = TRUE) # attributes can change through time
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
        if(values$nwnum == "multiple" & !("networkDynamic" %in% class(nw_var))){
          try({nw_var <- networkDynamic::networkDynamic(base.net = nw_var[[1]],
                                   network.list = nw_var,
                                   onsets = seq(from = 0, length = length(nw_var)),
                                   termini = seq(from = 1, length = length(nw_var)),
                                   verbose = FALSE,
                                   create.TEAs = FALSE) # change to TRUE if attributes change through time
          })
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
  if(!is.network(nw())){return()}
  nw()$gal$n
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
#remove ".active" suffix from networkDynamic objs
menuattr <- reactive({
  if(!is.network(nw())){return()}
  all.attrs <- unlist(strsplit(x = attrib(), split = ".active", fixed = TRUE))
  teas <- c()
  if("networkDynamic" %in% class(nw())){
    tea.ind <- grep(pattern = ".active", x = attrib(),
                    fixed = TRUE, value = FALSE)
    teas <- all.attrs[tea.ind]
  }
  if(is.element("na", all.attrs)){
    all.attrs <- all.attrs[-which("na" == all.attrs)]
  }
  if(is.element("vertex.names", all.attrs)){
    all.attrs <- all.attrs[-which("vertex.names" == all.attrs)]
  }
  if(is.element("active", all.attrs)){
    all.attrs <- all.attrs[-which("active" == all.attrs)]
  }

  menuattr <- list(all.attrs = all.attrs, teas = teas)
})

#numeric attributes only (for size menu, etc.)
numattr <- reactive({
  numattr <- c()
  if(is.network(nw())){
    for(i in 1:length(menuattr()$all.attrs)){
      if(is.numeric(get.vertex.attribute(nw(),menuattr()$all.attrs[i]))){
        numattr <- append(numattr,menuattr()$all.attrs[i])
      }
    }}
  numattr
})

#dataframe of nodes, their attributes
#list of dataframes for a networkDynamic object
nwdf <- reactive({
  attrs <- menuattr()$all.attrs
  if("networkDynamic" %in% class(nw())){
   df <- lapply(X = networkDynamic::get.change.times(nw()),
                FUN = function(time){
      if(is.na(as.numeric(network.vertex.names(nw()))[1])){
        dfslice <- data.frame(Names = network.vertex.names(nw()))
      } else {
        dfslice <- data.frame(Names = as.numeric(network.vertex.names(nw())))
      }
      for(i in seq(length(attrs))){
        if(attrs[i] %in% menuattr()$teas){
          dfslice[[attrs[i]]] <- networkDynamic::get.vertex.attribute.active(x = nw(),
                                                        prefix = attrs[i],
                                                        at = time)
        } else {
          dfslice[[attrs[i]]] <- get.vertex.attribute(nw(), attrs[i])
        }
      }
      dfslice[["Missing"]] <- get.vertex.attribute(nw(), "na")
      dfslice
    })
  } else {
    if(is.na(as.numeric(network.vertex.names(nw()))[1])){
      df <- data.frame(Names = network.vertex.names(nw()))
    } else {
      df <- data.frame(Names = as.numeric(network.vertex.names(nw())))
    }
    for(i in seq(length(attrs))){
      df[[attrs[i]]] <- get.vertex.attribute(nw(), attrs[i])
    }
    df[["Missing"]] <- get.vertex.attribute(nw(), "na")
  }
  df
})

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

observeEvent(input$percButton_dd, {
  state$plotperc_dd <- TRUE
})
observeEvent(input$countButton_dd, {
  state$plotperc_dd <- FALSE
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

  if("networkDynamic" %in% class(nw())){
    timeind <- networkDynamic::get.change.times(nw())[-1]
    # columns are nodes, rows are times
    deg <- tsna::tDegree(nw(), start = timeind[1] - 1,
                         end = timeind[length(timeind)],
                         cmode = input$cmode_dd)
    ldata <- lapply(timeind, FUN = function(x){
                data <- tabulate(deg[x,])
                data <- append(data, sum(deg[x,] == 0, na.rm = TRUE), after = 0)
              })
    maxdeg <- max(unlist(ldata), na.rm = TRUE)

#     #for color-coded bars
#     if(!is.null(input$colorby_dd) & input$colorby_dd != "None"){
#       if(is.directed(nw())){
#         if(input$cmode_dd == 'indegree'){
#           data <- summary(
#             nw() ~ idegree(0:maxdeg, input$colorby_dd), at = timeind - 1)
#         } else if(input$cmode_dd == 'outdegree'){
#           data <- summary(
#             nw() ~ odegree(0:maxdeg, input$colorby_dd), at = timeind - 1)
#         } else {
#           return('Cannot color code a directed graph using total degree.')
#         }
#       } else {
#         data <- summary(
#           nw() ~ degree(0:maxdeg, input$colorby_dd), at = timeind - 1)
#       }
#       ldata <- lapply(timeind, FUN = function(x){
#         data <- t(matrix(data[x,], nrow = maxdeg+1))
#         colnames(data) <- 0:maxdeg
#         })
#     }

  } else {
    deg <- sna::degree(nw(), gmode = gmode, cmode = input$cmode_dd, diag = diag)
    data <- tabulate(deg)
    data <- append(data, sum(deg == 0, na.rm = TRUE), after = 0)
    maxdeg <- max(deg, na.rm = TRUE)

    #for color-coded bars
    if(!is.null(input$colorby_dd) & input$colorby_dd != "None"){
      if(is.directed(nw())){
        if(input$cmode_dd == 'indegree'){
          data <- summary(nw() ~ idegree(0:maxdeg, input$colorby_dd))
        } else if(input$cmode_dd == 'outdegree'){
          data <- summary(nw() ~ odegree(0:maxdeg, input$colorby_dd))
        } else {
          return('Cannot color code a directed graph using total degree.')
        }
      } else {
        data <- summary(nw() ~ degree(0:maxdeg, input$colorby_dd))
      }
      data <- t(matrix(data, nrow = maxdeg+1))
      colnames(data) <- 0:maxdeg
    }
    ldata <- list(data)
  }
  ldata
})

frs <- reactive({
  if("networkDynamic" %in% class(nw())){
    timeind <- networkDynamic::get.change.times(nw())
    start <- timeind[1]
    end <- timeind[length(timeind)-1]
    frs <- c()
    #columns are nodes, rows are time steps
    frs <- vapply(1:nodes(), FUN = function(x){
      frssize <- c()
      for(i in start+1:end){
        size <- length(tsna::forward.reachable(nw(), v = x,
                                               start = start, end = i))
        frssize <- append(frssize, size)
      }
      frssize
    }, FUN.VALUE = rep(0, length(start+1:end)))
    frs
  }
})

## FIT MODEL ##

formation <- reactive({
  input$updateformulaButton
  isolate({input$formation})
})

dissolution <- reactive({
  if(values$nwnum == "single"){
    paste(input$dissolution, collapse = " + ")
  } else {
    input$updatedissButton
    isolate(input$dissolution2)
  }

})

observeEvent(input$resetformulaButton, {
  updateTextInput(session, "formation",
                  label = NULL, value = "edges")
})
observeEvent(nw(), {
  updateTextInput(session, "formation",
                  label = NULL, value = "edges")
})
observeEvent(input$resetdissButton, {
  updateTextInput(session, "dissolution2",
                  label = NULL, value = "")
})

formoffsets <- reactive({
  offterms <- grep("offset", strsplit(formation(), "+", fixed = TRUE)[[1]],
                   fixed = TRUE, value = TRUE)
  ncoefs <- length(offterms)
  ids <- paste0("fcoef", ncoefs)
  unlist(input[[ids]])
})

dissoffsets <- reactive({
  if(values$nwnum == "single"){
    ncoefs <- length(input$dissolution)
  } else {
    offterms <- grep("offset", strsplit(dissolution(), "+", fixed = TRUE)[[1]],
                     fixed = TRUE, value = TRUE)
    ncoefs <- length(offterms)
  }
  ids <- paste0("coef", ncoefs)
  unlist(input[[ids]])
})

targetstats <- reactive({
  eval(parse(text = paste("c(", input$target.stats, ")")))
})

CMLEtimes <- reactive({
  if(input$inclusivetimes){
    input$CMLEtimes[1]:input$CMLEtimes[2]
  } else {
    input$CMLEtimes
  }
})

estimate <- reactive({
  if("networkDynamic" %in% class(nw())){
    "CMLE"
  } else {
    "EGMME"
  }
})

stergmcontrols <- reactive({
  customcontrols <- isolate(paste(input$customMCMCcontrol, sep = ","))
  if(customcontrols == ""){
    control.stergm(CMLE.MCMC.burnin = input$MCMCburnin,
                   CMLE.MCMC.interval = input$MCMCinterval,
                   CMLE.control = control.ergm(
                     MCMLE.maxit = input$MCMLEmaxit,
                     MCMC.samplesize = input$MCMCsamplesize
                   ),
                   EGMME.MCMC.burnin.min = input$EGMME.burnin.min,
                   EGMME.MCMC.burnin.max = input$EGMME.burnin.max,
                   EGMME.MCMC.burnin.add = input$EGMME.burnin.add,
                   EGMME.MCMC.burnin.pval = input$EGMME.burnin.pval)
  } else {
    control.stergm(CMLE.MCMC.burnin = input$MCMCburnin,
                   CMLE.MCMC.interval = input$MCMCinterval,
                   EGMME.MCMC.burnin.min = input$EGMME.burnin.min,
                   EGMME.MCMC.burnin.max = input$EGMME.burnin.max,
                   EGMME.MCMC.burnin.add = input$EGMME.burnin.add,
                   EGMME.MCMC.burnin.pval = input$EGMME.burnin.pval,
                   eval(parse(text = customcontrols)))
  }
})

#stergm model object
stergm.fit <- reactive({
  if(input$fitButton == 0){
    return()
  }
  usingdefault <- isolate(input$controldefault)
  if(usingdefault){
    isolate({
      fit <- stergm(nw(),
                    formation = as.formula(paste("~", formation())),
                    dissolution = as.formula(paste("~", dissolution())),
                    targets = "formation",
                    target.stats = targetstats(),
                    times = CMLEtimes(),
                    offset.coef.form = formoffsets(),
                    offset.coef.diss = dissoffsets(),
                    estimate = estimate())
    })
  } else {
    isolate({
      fit <- stergm(nw(),
                    formation = as.formula(paste("~", formation())),
                    dissolution = as.formula(paste("~", dissolution())),
                    targets = "formation",
                    target.stats = targetstats(),
                    times = CMLEtimes(),
                    offset.coef.form = formoffsets(),
                    offset.coef.diss = dissoffsets(),
                    estimate = estimate(),
                    control = stergmcontrols())
    })
  }

  return(fit)
})



stergm.gof <- reactive({
  if(estimate() == "EGMME"){return()}
  gof(stergm.fit())
})


# Output Objects ----------------------------------------------------------

#to be able to check number of panels from the ui
output$nwnum <- renderText({
  values$nwnum
})

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
             "sampson", "windsurfers")
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
              label = 'Color vertices according to:',
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
              label = 'Size vertices according to:',
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
  ndtv::render.d3movie(nw(),
                 output.mode = "htmlWidget",
                 launchBrowser = FALSE,
                 displaylabels = input$vnames,
                 vertex.col = color,
                 vertex.sides = sides,
                 vertex.cex = vcex)
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

output$attrcheck <- renderUI({
    checkboxGroupInput("attrcols",
                       label = "Include these attributes",
                       choices = menuattr()$all.attrs,
                       selected = menuattr()$all.attrs)
})
outputOptions(output, "attrcheck", suspendWhenHidden = FALSE)

output$ndslices_tbl_ui <- renderUI({
  if("networkDynamic" %in% class(nw())){
    numericInput("ndslice_tbl",
                 label = "Network panel",
                 value = 0,
                 min = 0,
                 max = max(networkDynamic::get.change.times(nw())))
  }
})
outputOptions(output, "ndslices_tbl_ui", suspendWhenHidden = FALSE)

output$attrtbl <- renderDataTable({
  if("networkDynamic" %in% class(nw())){
    dflist <- nwdf()
    df <- dflist[[input$ndslice_tbl + 1]]
    dt <- df[, c("Names", input$attrcols)]
  } else {
    dt <- nwdf()[, c("Names", input$attrcols)]
  }
  dt
}, options = list(pageLength = 10))

output$attrplots <- renderPlot({
  nplots <- length(input$attrcols)
  if(nplots == 0){return()}
  attrname <- input$attrcols
  if("networkDynamic" %in% class(nw())){

    slices <- networkDynamic::get.change.times(nw())
    if(nplots == 1){
      lvls <- length(unique(nwdf()[[attrname]]))
      cols <- RColorBrewer::brewer.pal(9, "Set1")
      par(mfrow = c(1, 1))
      template <- attr.info(df = nwdf()[[1]], colname = attrname,
                            numattrs = numattr(), breaks = 10)/2
      # get matrix of attribute totals
      # each column is a nw panel, each row is an attr value
      attrcounts <- vapply(nwdf(), FUN = function(x){
        tab <- attr.info(x, attrname, numattrs = numattr(), breaks = 10)
        if(input$attrhistaxis == "percent"){
          tab <- tab/sum(tab)
        }
        tab
      }, FUN.VALUE = template)

      plot(x = slices,
           y = attrcounts[1,],
           type = "l", lwd = 2, col = cols[1],
           ylim = c(min(attrcounts)-1, max(attrcounts)+1),
           xlab = "Time panel",
           ylab = "Attribute totals",
           main = attrname)
      for(i in 2:nrow(attrcounts)){
        lines(x = slices,
              y = attrcounts[i,], lwd = 2, col = cols[i])
      }
      legend(x = "topright", legend = row.names(attrcounts),
             fill = cols)

    } else if (nplots > 1) {
      r <- ceiling(nplots/2)
      par(mfrow = c(r, 2))
      for(a in attrname){
        lvls <- length(unique(nwdf()[[a]]))
        cols <- RColorBrewer::brewer.pal(9, "Set1")
        template <- attr.info(df = nwdf()[[1]], colname = a,
                              numattrs = numattr(), breaks = 10)/2
        # get matrix of attribute totals
        # each column is a nw panel, each row is an attr value
        attrcounts <- vapply(nwdf(), FUN = function(x){
          tab <- attr.info(x, a, numattrs = numattr(), breaks = 10)
          if(input$attrhistaxis == "percent"){
            tab <- tab/sum(tab)
          }
          tab
        }, FUN.VALUE = template)
        plot(x = slices,
             y = attrcounts[1,],
             type = "l", lwd = 2, col = cols[1],
             ylim = c(min(attrcounts)-.5, max(attrcounts)+.5),
             xlab = "Time panel",
             ylab = "Attribute totals",
             main = a)
        for(i in 2:nrow(attrcounts)){
          lines(x = slices,
                y = attrcounts[i,], lwd = 2, col = cols[i])
        }
        legend(x = "topright", legend = row.names(attrcounts),
               fill = cols)
      }
    }

  } else {

    if(nplots == 1){
      par(mfrow = c(1, 1))
      lvls <- length(unique(nwdf()[[attrname]]))
      tab <- attr.info(df = nwdf(), colname = attrname,
                       numattrs = numattr(), breaks = 10)
      if(input$attrhistaxis == "percent"){
        tab <- tab/sum(tab)
      }
      barplot(tab, main = attrname, col = histblue)

    } else {
      r <- ceiling(nplots/2)
      par(mfrow = c(r, 2))
      for(a in attrname){
        tab <- attr.info(df = nwdf(), colname = a,
                         numattrs = numattr(), breaks = 10)
        if(input$attrhistaxis == "percent"){
          tab <- tab/sum(tab)
        }
        barplot(tab, main = a, col = histblue)

      }
    }
  }

})

output$attrplotspace <- renderUI({
  nplots <- length(input$attrcols)
  r <- ceiling(nplots/2)
  h <- ifelse(r == 1, 400, r * 300)
  plotOutput("attrplots", height = h)
})

#DEGREE DISTRIBUTION

output$dynamiccmode_dd <- renderUI({
  menu <- c()
  if(is.network(nw())){
    menu <- c("total" = "freeman")
    if(is.directed(nw())){
      menu <- c("total" = "freeman",
                "indegree",
                "outdegree")
    }
  }
  selectInput("cmode_dd",
              label = "Type of degree",
              choices = menu)
})
outputOptions(output,'dynamiccmode_dd',suspendWhenHidden=FALSE, priority=10)

output$dynamiccolor_dd <- renderUI({
  menu <- menuattr()$all.attrs
  if(any(menu %in% menuattr()$teas)){
    menu <- menu[-which(menu %in% menuattr()$teas)]
  }
  if(is.network(nw())){
    if(input$cmode_dd == "freeman" & is.directed(nw())){
      menu <- c()
    }
    selectInput('colorby_dd',
                label = 'Color bars according to:',
                c('None', menu),
                selected = 'None')
  }
})
outputOptions(output,'dynamiccolor_dd',suspendWhenHidden=FALSE, priority=10)

output$ndslices_dd_ui <- renderUI({
  npanels <- length(dd_plotdata())
  numericInput("ndslice_dd",
               label = "Network panel",
               value = 0,
               min = 0,
               max = npanels - 1)
})

output$degreedist <- renderPlot({
  if(!is.network(nw())){
    return()
  }
  input$plottabs
  input$rawdatafile
  input$samplenet

  ylabel <- "Count of Vertices"
  xlabel <- "Degree"
  if(input$cmode_dd == "indegree"){
    xlabel <- "In Degree"
  } else if (input$cmode_dd == "outdegree"){
    xlabel <- "Out Degree"
  }

  try({plotme <- dd_plotdata()[[input$ndslice_dd + 1]]})
  color <- histblue
  ltext <- c()
  lcol <- c() #color for lines
  lty <- c()
  lpch <- c()
  lfill <- c() #color for boxes
  lborder <- c()
  ltitle <- NULL

    if(!is.null(input$colorby_dd)){
      if(input$colorby_dd != "None"){
        validate(need(!("networkDynamic" %in% class(nw())),
                      "Color coding by attribute is not yet available for networkDynamic objects"))
        ncolors <- dim(plotme)[1]
        if(ncolors == 2){
          color <- c("#eff3ff", "#377FBC")
        } else if(ncolors < 10){
          color <- RColorBrewer::brewer.pal(ncolors,"Blues")
        } else if(ncolors >= 10){
          color <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(ncolors)
        }
        ltext <- sort(unique(get.vertex.attribute(nw(),input$colorby_dd)))
        ltext <- append(ltext, "")
        lfill <- c(color, 0)
        lborder <- append(lborder, c(rep("black", times=ncolors), 0))
        lty <- rep(0, times=ncolors+1)
        lpch <- rep(26, times=ncolors+1)
        ltitle <- input$colorby_dd
      }}

  # get maximums for y limits of plot
  if(class(plotme) == "matrix"){
    maxfreq <- max(colSums(plotme))
    maxdeg_obs <- dim(plotme)[2] - 1
    names(plotme) <- paste(0:maxdeg_obs)
  } else {
    maxfreq <- max(plotme, na.rm = TRUE)
    maxdeg_obs <- length(plotme) - 1
    names(plotme) <- paste(0:maxdeg_obs)
  }

  if(state$plotperc_dd) {
    plotme <- plotme/sum(plotme)
    ylabel <- 'Percent of Vertices'
  }

  barplot(plotme, xlab = xlabel, ylab = ylabel,
          col = color, plot = TRUE)

  if(input$colorby_dd != "None" ){
    lmerge <- FALSE
    lpch <- NULL
    legend(x = "topright", legend = ltext, title = ltitle, fill = lfill,
           border = lborder, col = lcol, lty = lty, pch = lpch, pt.cex = 1.25,
           bty = "n", merge = lmerge)
  }

})

# MORE

output$durplot <- renderPlot({
  boxplot(list(edges = tsna::edgeDuration(nw()),
               vertices = tsna::vertexDuration(nw())),
          ylab = "Time Steps",
          border = obsblue,
          col = adjustcolor(obsblue, alpha.f = 0.3),
          main = "Durations")
})
outputOptions(output, "durplot", suspendWhenHidden = FALSE)

output$tstatterm_ui <- renderUI({
  if(!is.network(nw())){return()}
  if(is.directed(nw())){
    choices <- c("cyclicalties", "density", "isolates", "mean degree" = "meandeg",
                 "mixed 2-stars" = "m2star", "mutual",
                 "transitive triads" = "transitive", "triangle", "ttriple",
                 "twopath")
  } else {
    choices <- c("density", "concurrent", "isolates", "mean degree" = "meandeg",
                 "triangle")
  }
  selectizeInput("tstatterm", label = NULL,
                 choices = c("Choose one or more terms" = "", choices),
                 selected = "density",
                 multiple = TRUE)
})

output$tstatplot <- renderPlot({
  if(is.null(input$tstatterm)){return()}
  if(input$tstatterm == ""){return()}
  timeind <- networkDynamic::get.change.times(nw())
  timeind <- timeind[-length(timeind)]
  statform <- paste("~", paste(input$tstatterm, collapse = "+"))
  dat <- tsna::tErgmStats(nw(), statform, start = timeind[1],
                          end = length(timeind)-1)
  nlines <- dim(dat)[2]
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  plot(x = timeind, y = dat[,1], type = "l", lwd = 2,
       ylim = c(min(dat), max(dat)), col = cols[1],
       xlab = "Time Panel", ylab = "Network Statistic Value")
  if(nlines >= 2){
    for(i in 2:nlines){
      lines(x = timeind, y = dat[,i], lwd = 2, col = cols[i])
    }
  }
  legend(x = "topright", legend = input$tstatterm,
         lwd = 2, col = cols[1:nlines])

})

output$frs_ui1 <- renderUI({
  timeind <- networkDynamic::get.change.times(nw())
  column(3,
    numericInput("frsnode",
                 label = "Node ID",
                 value = 1,
                 min = 1,
                 max = nw()$gal$n)
  )
})

output$frs_ui2 <- renderUI({
  timeind <- networkDynamic::get.change.times(nw())
  column(3,
     numericInput("frsstart",
                  label = "Start time step",
                  value = 0,
                  min = 0,
                  max = timeind[length(timeind)-2])
  )
})

output$frs_ui3 <- renderUI({
  input$frsstart
  timeind <- networkDynamic::get.change.times(nw())
  column(3,
         numericInput("frsend",
                      label = "End time step",
                      value = 1 + input$frsstart,
                      min = 1 + input$frsstart,
                      max = timeind[length(timeind)-1])
  )
})

output$frsnodeset <- renderPrint({
  setvec <- tsna::forward.reachable(nw(), v = input$frsnode,
                          start = input$frsstart, end = input$frsend)
  cat(paste("FRS from node", input$frsnode, ":"), setvec)
})

output$frsplot <- renderPlot({
  if(input$frsButton == 0){return()}
  isolate({
    timeind <- networkDynamic::get.change.times(nw())
    timeind <- timeind[-length(timeind)]
    if(input$frslines){
      plot(x = timeind[-1], y = frs()[,1], type = "l", col = obsblue,
           xlab = "Time Step", ylab = "", main = "Size of Forward Reachable Set")
      for(p in 2:dim(frs())[2]){
        lines(x = timeind[-1], y = frs()[,p], col = obsblue)
      }
    } else {
      mn <- rowMeans(frs())
      ql <- vapply(seq(nrow(frs())), function(x){quantile(frs()[x,], 0.25)}, 1)
      qu <- vapply(seq(nrow(frs())), function(x){quantile(frs()[x,], 0.75)}, 1)
      plot(x = timeind[-1], y = mn, type = "l", lwd = 2, col = obsblue,
           xlab = "Time Step", ylab = "", main = "Size of Forward Reachable Set")
      polygon(x = c(timeind[-1], rev(timeind[-1])), y = c(qu, rev(ql)),
              col = adjustcolor(obsblue, alpha.f = .3), border = NA)
    }
  })

})

# update all the menu selection options for descriptive indices when network changes
observeEvent(nw(), {
  if(is.network(nw())){
    if(is.directed(nw())){
      degmenu <- c('indegree', 'outdegree')
      betwmenu <- c('directed', 'endpoints', 'proximalsrc',
                    'proximaltar', 'proximalsum', 'lengthscaled', 'linearscaled')
      closemenu <- c('directed', 'suminvdir')
      stressmenu <- c('directed')
      hgmenu <- c('directed')
    } else {
      degmenu <- c('total')
      betwmenu <- c('undirected', 'endpoints', 'proximalsrc',
                    'proximaltar', 'proximalsum', 'lengthscaled', 'linearscaled')
      closemenu <- c('undirected', 'suminvundir')
      stressmenu <- c('undirected')
      hgmenu <- c('undirected')
    }
    updateNumericInput(session, "nodeind", label = NULL, value = 1,
                       min = 1, max = nodes())
    updateSelectInput(session, "gdegcmode", choices = degmenu)
    updateSelectInput(session, "gbetwcmode", choices = betwmenu)
    updateSelectInput(session, "gclosecmode", choices = closemenu)
    updateSelectInput(session, "gstresscmode", choices = stressmenu)
    updateSelectInput(session, "ggraphcentcmode", choices = hgmenu)
    updateSelectInput(session, "ndegcmode", choices = degmenu)
    updateSelectInput(session, "nbetwcmode", choices = betwmenu)
    updateSelectInput(session, "nclosecmode", choices = closemenu)
    updateSelectInput(session, "nstresscmode", choices = stressmenu)
    updateSelectInput(session, "ngraphcentcmode", choices = hgmenu)
  }
})

output$gden <- renderText({
  if(!is.network(nw())) {return()}
  if(is.directed(nw())){
    gmode <- 'digraph'
  } else {
    gmode <- 'graph'
  }
  sna::gden(nw(), diag=has.loops(nw()), mode=gmode)
})
outputOptions(output,'gden',suspendWhenHidden=FALSE)

output$grecip <- renderText({
  if(!is.network(nw())) {return()}
  if(input$grecipmeas == ''){
    return()
  }
  try(sna::grecip(nw(), measure=input$grecipmeas))
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
  try(sna::gtrans(nw(), diag=has.loops(nw()), mode=gmode,
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
  d <- ""
  cmode <- input$gdegcmode
  if(cmode == 'total'){
    cmode <- 'freeman'
  }
  try(d <- sna::centralization(nw(), sna::degree, mode=gmode, diag=has.loops(nw()),
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
  b <- ""
  try(b <- sna::centralization(nw(), sna::betweenness, mode=gmode, diag=has.loops(nw()),
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
  c <- ""
  try(
    c <- sna::centralization(nw(), sna::closeness, mode=gmode, diag=has.loops(nw()),
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
  s <- ""
  try(s <- sna::centralization(nw(), sna::stresscent, mode=gmode, diag=has.loops(nw()),
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
  g <- ""
  try(g <- sna::centralization(nw(), sna::graphcent, mode=gmode, diag=has.loops(nw()),
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
  e <- ""
  try(e <- sna::centralization(nw(), sna::evcent, mode=gmode, diag=has.loops(nw())))
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
  i<-""
  try({
    i <- sna::centralization(nw(), sna::infocent, mode=gmode, diag=has.loops(nw()),
                             cmode=input$ginfocentcmode)})
  i
})
outputOptions(output,'ginfocent',suspendWhenHidden=FALSE)


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

output$formcoefs <- renderUI({
  offterms <- grep("offset", strsplit(formation(), "+", fixed = TRUE)[[1]],
                   fixed = TRUE, value = TRUE)
  ncoefs <- length(offterms)
  if(ncoefs == 0) {return()}
  ids <- paste0("fcoef", seq(ncoefs))
  div(class = "skinny",
      strong("Offset coefficient value(s):"),
      lapply(ids, FUN = numericInput, value = 1, label = NULL),
      title = "Enter coefficients values in the same order offset terms appear in the formation formula",
      style = "margin-top: 5px;"
  )
})

output$CMLEtimes_ui <- renderUI({
  times <- networkDynamic::get.change.times(nw())
  sliderInput("CMLEtimes", label = NULL,
              min = times[1], max = times[length(times)],
              value = times[1:2], step = 1, dragRange = TRUE)
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
  if(values$nwnum == "single"){
    ncoefs <- length(input$dissolution)
  } else {
    offterms <- grep("offset", strsplit(dissolution(), "+", fixed = TRUE)[[1]],
                     fixed = TRUE, value = TRUE)
    ncoefs <- length(offterms)
  }
  if(ncoefs == 0) {return()}
  ids <- paste0("coef", seq(ncoefs))
  div(class = "skinny",
      strong("Offset coefficient value(s):"),
      lapply(ids, FUN = numericInput, value = 1, label = NULL),
      title = "Enter coefficients values in the same order offset terms appear in the formation formula",
      style = "margin-top: 5px;"
  )
})

output$prefitsum <- renderPrint({
  if(!is.network(nw()) | length(input$formation) == 0){
    return(cat('NA'))
  }
  options(width = 140)
  f <- as.formula(paste("nw() ~", formation()))
  if(estimate() == "EGMME"){
    summary(as.formula(paste("nw() ~", formation())))
  } else {
    nterms <- length(attr(terms(f), which = "term.labels")) +
      length(attr(terms(f), which = "offset"))
    summ <- summary(f, at = CMLEtimes())
    if(nterms == 1){
      colnames(summ) <- ""
    } else if(nterms > 1) {
      rownames(summ) <- paste("time", CMLEtimes())
    }
    summ
  }

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

output$gofsumform <- renderPrint({
  stergm.gof()$formation
})

output$gofsumdiss <- renderPrint({
  stergm.gof()$dissolution
})

output$gofplotform <- renderPlot({
  if(is.directed(nw())){
    par(mfrow = c(4,1))
  } else {
    par(mfrow = c(3,1))
  }
  plot(stergm.gof()$formation)
}, height = 1200)

output$gofplotdiss <- renderPlot({
  if(is.directed(nw())){
    par(mfrow = c(4,1))
  } else {
    par(mfrow = c(3,1))
  }
  plot(stergm.gof()$dissolution)
}, height = 1200)

})

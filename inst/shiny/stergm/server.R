
## server.R for stergm app

library(statnet)
library(tergm)

data(faux.mesa.high)
data(florentine)
data(sampson)
data(samplk)
data(ecoli)
data(molecule)
data(kapferer)

shinyServer(
  function(input, output, session){

# Reactive Values ---------------------------------------------------------
## Roughly in order of use


values <- reactiveValues()

# when two options are available to the user, or when we need to know if one
# variable is outdated this reactive value will keep track of the state
state <- reactiveValues(symmdir = FALSE, plotperc_dd = FALSE,
                        plotperc_gd = FALSE, allterms = FALSE, gof = 0)

# To keep a list of all attributes uploaded by the user:
values$v_attrNamesToAdd <- list(1)
values$v_attrValsToAdd <- list()
values$e_attrNamesToAdd <- list(1)
values$e_attrValsToAdd <- list()
values$ev_attrNamesToAdd <- list(1)
values$ev_attrValsToAdd <- list()
values$input_termslist <- list()


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
leftarrowclicks <- reactive({
  input$dataleft+input$plotleft+input$fitleft
})
rightarrowclicks <- reactive({
  input$dataright+input$plotright+input$fitright
})
observe({
  if(leftarrowclicks() == 0) {return()}
  tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
  current <- isolate(which(input$navbar == tabOptions))
  updateTabsetPanel(session, 'navbar', selected = tabOptions[current-1])
})
observe({
  if(rightarrowclicks() == 0) {return()}
  tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
  current <- isolate(which(input$navbar == tabOptions))
  updateTabsetPanel(session, 'navbar', selected = tabOptions[current+1])
})

#nwinit is used to get the initial values of the network
nwinit <- reactive({
  #input$rawdatafile comes as a dataframe with name, size, type and datapath
  #datapath is stored in 4th column of dataframe
  #network creates a network object from the input file
  if(is.null(input$rawdatafile)){
    nw_var <- NULL
  } else {
    filepath <- input$rawdatafile[1,4]
    filename <- input$rawdatafile[1,1]
    fileext <- substr(filename, nchar(filename)-3, nchar(filename))

    if(input$filetype == 2){
      validate(
        need(fileext %in% c(".rds", ".Rds", ".RDs", ".RDS"),
             "Upload an .rds file"))
      nw_var <- readRDS(paste(filepath))
    } else if(input$filetype == 4){
      validate(
        need(fileext %in% c(".net", ".NET"),
             "Upload a .net file"))
      nw_var <- read.paj(paste(filepath))
    } else if(input$filetype == 5){
      validate(
        need(fileext %in% c(".paj",".PAJ"),
             "Upload a .paj file"))
      nws <- read.paj(paste(filepath))
      if(!is.null(pajnws())){
        nw_var <- nws$networks[[as.numeric(input$choosepajnw)]]
      }
    } else if(input$filetype == 3){
      validate(
        need(fileext %in% c(".csv",".CSV") |
               fileext %in% c(".rds", ".Rds", ".RDs", ".RDS"),
             "Upload the specified type of matrix"))
      if(fileext %in% c(".csv",".CSV")){
        header <- TRUE
        row_names <- 1
        if(input$matrixtype == "edgelist"){
          header <- FALSE
          row_names<-NULL
        }
        try({nw_var <- network(read.csv(paste(filepath), sep = ",",
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

      } else if(fileext %in% c(".rds", ".Rds", ".RDs", ".RDS")){
        newmx <- readRDS(paste(filepath))
        nw_var <- network(newmx,
                          directed = input$dir,
                          loops = input$loops,
                          multiple = input$multiple,
                          bipartite = input$bipartite,
                          matrix.type = input$matrixtype,
                          ignore.eval = FALSE,
                          names.eval = 'edgevalue')

      }
    }
  }
  if(input$filetype == 1){
    if(input$samplenet == ""){
      nw_var <- NULL
    } else {
      nw_var <- eval(parse(text = input$samplenet))
      if(!is.element('bipartite', names(nw_var$gal))){
         set.network.attribute(nw_var, 'bipartite', FALSE)
      }
    }
  }
  return(nw_var)
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

#number of edges in initial nw
nedgesinit <- reactive({
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
  v <- list()
  for (j in seq(length(list.vertex.attributes(nwinit())))) {
    v[[j]] <- get.vertex.attribute(nwinit(), vattrinit()[j])
  }
  v
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
    e <- nedgesinit()
    for (i in 1:n){
      vdf <- rbind(vdf,i)
    }
    for (i in 1:e){
      edf <- rbind(edf,i)
      evdf <- rbind(evdf,i)
    }
    values$v_attrValsToAdd <- vdf
    values$e_attrValsToAdd <- edf
    values$ev_attrValsToAdd <- evdf
    values$v_attrNamesToAdd <- list(1)
    values$e_attrNamesToAdd <- list(1)
    values$ev_attrNamesToAdd <- list(1)

    values$vertexnames <- network.vertex.names(nwinit())
  }
})

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

#save new vertex names
observeEvent(input$newattrButton, {
  if(input$newattrtype == "vertexnames"){
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]
    fileext <- substr(filename,nchar(filename)-3,nchar(filename))
    if(fileext %in% c(".csv", ".CSV")){
      newnames <- read.csv(paste(path), sep=",", header=TRUE,
                           stringsAsFactors=FALSE)
      newnames <- newnames[[1]]
    } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
      newnames <- readRDS(paste(path))
    }

    values$vertexnames <- newnames
  }
})

#add vertex attributes to list
observeEvent(input$newattrButton, {
  if(input$newattrtype == "vertexattr"){
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]
    fileext <- substr(filename,nchar(filename)-3,nchar(filename))
    if(fileext %in% c(".csv", ".CSV")){
      newattrs <- read.csv(paste(path), sep=",", header=TRUE,
                           stringsAsFactors=FALSE)
      newname <- names(newattrs)
    } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
      newattrs <- readRDS(paste(path))
      newname <- names(newattrs)
    }

    namesofar <- values$v_attrNamesToAdd
    valsofar <- values$v_attrValsToAdd
    for(k in 1:length(newname)){
      namesofar <- cbind(namesofar, newname[[k]])
      valsofar <- cbind(valsofar, newattrs[[k]])
    }

    values$v_attrNamesToAdd <- namesofar
    values$v_attrValsToAdd <- valsofar
  }
})

#add edge attributes to list
observeEvent(input$newattrButton, {
  if(input$newattrtype == "edgeattr" & input$edgeform == "vector"){
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]
    fileext <- substr(filename,nchar(filename)-3,nchar(filename))
    if(fileext %in% c(".csv", ".CSV")){
      newattrs <- read.csv(paste(path), sep=",", header=TRUE,
                           stringsAsFactors=FALSE)
      newname <- names(newattrs)
    } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
      newattrs <- readRDS(paste(path))
      newname <- names(newattrs)
    }

    namesofar <- values$e_attrNamesToAdd
    valsofar <- values$e_attrValsToAdd
    for(k in 1:length(newname)){
      namesofar <- cbind(namesofar, newname[[k]])
      valsofar <- cbind(valsofar, newattrs[[k]])
    }
    values$e_attrNamesToAdd <- namesofar
    values$e_attrValsToAdd <- valsofar
  }
})

#add edge values to list
observeEvent(input$newattrButton, {
  if(input$newattrtype == "edgeattr" & input$edgeform == "matrix"){
    path <- input$newattrvalue[1,4]
    filename <- input$newattrvalue[1,1]
    fileext <- substr(filename,nchar(filename)-3, nchar(filename))
    if(fileext %in% c(".csv", ".CSV")){
      newattrs <- read.csv(paste(path), sep=",", header=TRUE,
                           row.names = 1,
                           stringsAsFactors=FALSE)
      newname <- substr(filename, 1, nchar(filename)-4)
      newattrs <- data.matrix(newattrs, rownames.force=FALSE)
    } else if(fileext %in% c(".rds",".Rds",".RDs",".RDS")){
      newattrs <- readRDS(paste(path))
      newname <- names(newattrs)
    }
    namesofar <- values$ev_attrNamesToAdd
    valsofar <- values$ev_attrValsToAdd
    j <- length(valsofar)
    for(k in 1:length(newname)){
      namesofar <- cbind(namesofar, newname[[k]])
      valsofar[[j+k]] <- newattrs[[k]]
    }
    values$ev_attrNamesToAdd <- namesofar
    values$ev_attrValsToAdd <- valsofar
  }
})

observeEvent(input$symmdir,{
  state$symmdir <- TRUE
})
observeEvent(input$symmundir,{
  state$symmdir <- FALSE
})

#attributes will be added to this network
nwmid <- reactive({
  nw_var <- nwinit()

  if (class(nw_var)=="network"){
    #preserve initial network attributes and let user choose if directed
    #after symmetrizing
    if(input$symmetrize != "Do not symmetrize"){
      symnw <- sna::symmetrize(nw_var, rule=input$symmetrize)
      nw_var <- network(symnw, matrix.type="adjacency", directed=state$symmdir,
                        hyper=nwattrinit()[2], loops=nwattrinit()[3],
                        multiple=nwattrinit()[4], bipartite=nwattrinit()[5])
      #add initial vertex attributes back after symmetrizing
      #can't add edge attributes back because number of edges has changed
      for(k in 1:length(vattrinit())){
        attr_names <- vattrinit()
        attr_list <- vattrinit.vals()
        set.vertex.attribute(nw_var, attr_names[k], attr_list[[k]])
      }
    }

    if (is.bipartite(nw_var)){
      set.vertex.attribute(nw_var, "mode", c(rep(1, nw_var$gal$bipartite),
                                             rep(2, nw_var$gal$n - nw_var$gal$bipartite)))
    }

    v_attrNamesToAdd <- values$v_attrNamesToAdd
    v_attrValsToAdd <- values$v_attrValsToAdd
    e_attrNamesToAdd <- values$e_attrNamesToAdd
    e_attrValsToAdd <- values$e_attrValsToAdd
    ev_attrNamesToAdd <- values$ev_attrNamesToAdd
    ev_attrValsToAdd <- values$ev_attrValsToAdd


    if(input$newattrButton > 0){
      try({network.vertex.names(nw_var) <- values$vertexnames})
    }
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

  values$input_termslist <- list()
  updateTextInput(session, inputId = 'terms', value = 'edges')

  nw_var
})

elist <- reactive({
  if(!is.network(nwinit())) return()
  as.edgelist(nw())
})

#get coordinates to plot network with
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


# Output Objects ----------------------------------------------------------

output$rawdatafile <- renderPrint({
  raw <- matrix(nrow = 2, ncol = 1)
  rownames(raw)<-c("name:", "size:")
  if(!is.null(input$rawdatafile)){
    raw[1, 1] <- input$rawdatafile[1, 1]
    raw[2, 1] <- paste(input$rawdatafile[1, ], " bytes")
  }
  write.table(raw, quote = FALSE, col.names = FALSE)})

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
  if (is.null(nw())){
    return(cat('NA'))
  }
  nw_var <- nw()
  if (class(nw_var) != "network"){
    return(cat(nw_var))
  }
  return(nw_var)
})

})

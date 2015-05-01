library(statnet)

# version of textInput with more size options
# specify class = "input-small" or class="input-mini" in addition to other textInput args
customTextInput<-function (inputId, label, value="", labelstyle="dispay:inline;",...) {
  tagList(
    tags$label(label, `for` = inputId, style=labelstyle), 
    tags$input(id = inputId, type="text", value=value,...))
}

customNumericInput<-function (inputId, label, value=0, labelstyle="display:inline;", ...) {
  tagList(
    tags$label(label, `for` = inputId, style=labelstyle), 
    tags$input(id = inputId, type="number", value=value,...))
}

#version of selectInput...shorter box and label inline
#lapply allows us to add each element of choices as an option in the select menu
inlineSelectInput<-function (inputId, label, choices,...) {
  tagList(
    tags$label(label, `for` = inputId, style="display:inline"), 
    tags$select(id = inputId, choices=choices,..., 
                style="width:100px; line-height:20px; font-size:12px",
                class="shiny-bound-input",
                lapply(choices, tags$option)))
}

#create a list of unique term names
sink("NUL") # prevents terms from printing to console
allterms <- search.ergmTerms()
sink()
inds <- regexpr(pattern='\\(', allterms)
for(i in 1:length(allterms)){
  allterms[i] <- substr(allterms[[i]], start=1, stop=inds[i]-1)
}
allterms <- unique(allterms)

#disable widgets when they should not be usable
disableWidget <- function(id, session, disabled=TRUE){
  if(disabled){
    session$sendCustomMessage(type="jsCode",
                              list(code=paste("$('#",id,"').prop('disabled',true)",
                                              sep="")))
  } else {
    session$sendCustomMessage(type="jsCode",
                              list(code=paste("$('#",id,"').prop('disabled',false)",
                                              sep="")))
  }
}

# function to return tests on simulated graphs
cugsims <- function(nw, term) {
  
  n <- nw$gal$n
  s <- network.edgecount(nw)
  if(nw$gal$directed){
    mode <- "digraph"
  } else {
    mode <- "graph"
  }
  dens <- gden(nw, mode = mode)
  
  simvals <- NULL
  
  for (i in 1:500){
      
    Ybrg <- matrix(rbinom(n^2, 1, dens), n, n)
    if(nw$gal$loops == FALSE){
      diag(Ybrg) <- NA
    }
    if(mode == "graph"){
      Ybrg <- symmetrize(Ybrg, rule = "upper")
    }
    
      
    Ycug <- matrix(0, n, n)
    diag(Ycug) <- NA
    if(mode == "graph"){
      Ycug[upper.tri(Ycug)] <- sample(c(rep(1, s), rep(0, n*(n-1)/2-s)))
      if(nw$gal$loops){
        Ycug[upper.tri(Ycug, diag = TRUE)] <- sample(c(rep(1, s), 
                                                       rep(0, n*(n-1)/2-s+n)))
      }
      Ycug <- symmetrize(Ycug, rule = "upper")
    } else {
      Ycug[!is.na(Ycug)] <- sample(c(rep(1,s), rep(0,n*(n-1)-s)))
      if(nw$gal$loops){
        Ycug <- matrix(sample(c(rep(1, s), rep(0, n*n - s))), n, n)
      }
    }
      
  nwcug <- network(Ycug, directed = nw$gal$directed, loops = nw$gal$loops)
  nwbrg <- network(Ybrg, directed = nw$gal$directed, loops = nw$gal$loops)
  val <- c(summary.formula(as.formula(paste("nwcug ~", term))),
           summary.formula(as.formula(paste("nwbrg ~", term))))
  simvals <- rbind(simvals, val)
  }
  return(simvals)
}

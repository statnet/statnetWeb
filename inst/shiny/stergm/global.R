
# create a list of unique term names
splitargs <- function(searchterm, nw){
  sink("NUL")
  allterms <- search.ergmTerms(keyword = searchterm, net = nw)
  sink()
  ind1 <- regexpr(pattern = "\\(", allterms)
  ind2 <- regexpr(pattern = "\\)", allterms)
  termnames <- substr(allterms, start = rep(1, length(allterms)), stop = ind1 - 1)
  termargs <- substr(allterms, start = ind1, stop = ind2)
  dups <- duplicated(termnames)
  termargs <- termargs[-which(dups)]
  termnames <- unique(termnames)
  list(names = termnames, args = termargs)
}

attr.info <- function(df, colname, numattrs, breaks) {
  lvls <- length(unique(df[[colname]]))
  if(colname %in% numattrs & lvls > 9){
    tab <- hist(df[[colname]], breaks = breaks, plot = FALSE)
    barname <- paste(tab$breaks[1:2], collapse = "-")
    for(i in seq(length(tab$breaks) - 2)){
      barname <- append(barname, paste(tab$breaks[i+1]+1,
                                       tab$breaks[i+2], sep = "-"))
    }
    tab <- tab$counts
    names(tab) <- barname
  } else {
    tab <- table(df[[colname]])
  }
  return(tab)
}
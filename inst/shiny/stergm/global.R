
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
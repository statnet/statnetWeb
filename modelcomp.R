# takes an ergm object and gathers some of the information from
# `summary.ergm`, in preparation to be passed to `model.comparison`

ergminfo <- function(object) {
  coefs <- object$coef
  terms <- names(object$coef)
  coefmatrix <- summary(object)$coefs
  pval <- coefmatrix[,4]
  signif.stars <- symnum(pval, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "), legend=F)
  starredcoef <- paste0(coefs, signif.stars)
  
  ans <- list()
  count <-1
  for(i in terms){
    ans[i] <- starredcoef[count]
    count <- count + 1
  }
  ans$aic <- summary(object)$aic
  ans$bic <- summary(object)$bic
  ans
}

# takes in a list of multiple outputs from ergminfo,
# formatting it all into a table comparing up to 5 models

model.comparison <- function(listobj) {
  n <- length(listobj)
  if(n>5) {stop("List of length greater than 5 passed to model.comparison")}
  allterms <- c()
  for(j in n){
    allterms <- append(allterms, names(listobj[[j]]))
  }
  allterms <- sort(unique(allterms))
  
}
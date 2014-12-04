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
  starredcoef <- paste0(format(coefs, digits=3), signif.stars)
  
  ans <- list()
  count <-1
  for(i in terms){
    ans[i] <- starredcoef[count]
    count <- count + 1
  }
  ans$AIC <- format(summary(object)$aic, digits=3)
  ans$BIC <- format(summary(object)$bic, digits=3)
  ans
}

# takes in a list of multiple outputs from ergminfo,
# formatting it all into a table comparing up to 5 models

model.comparison <- function(listobj) {
  n <- length(listobj)
  if(n>5) {stop("List of length greater than 5 passed to model.comparison")}
  allterms <- c()
  for(j in 1:n){
    allterms <- append(allterms, names(listobj[[j]]))
  }
  allterms <- sort(unique(allterms))
  allterms <- c(allterms[-which(allterms=="AIC")],"AIC")
  allterms <- c(allterms[-which(allterms=="BIC")],"BIC")
  mat <- matrix(nrow=length(allterms),ncol=n)
  for(k in 1:n){
    row <- 1
    for(l in allterms){
      if(is.null(listobj[[k]][[l]])){
        mat[row,k] <- "NA"
      } else {
        mat[row,k] <- listobj[[k]][[l]]
      }
      row <- row+1
    }
  }
  
  rownames(mat) <- allterms
  colnames(mat) <- paste0("Model",1:n )
  return(print(mat, quote=F))
}
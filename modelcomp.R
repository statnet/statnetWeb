# takes an ergm object and gathers some of the information from
# `summary.ergm`, in preparation to be passed to `model.comparison`

ergmsummary_short <- function(object) {
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
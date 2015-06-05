
#' @title ergm object information
#'
#' @description Takes an ergm object and gathers some of the information from 
#' \code{\link{summary.ergm}}, in preparation to be passed to the function 
#' \code{\link{coef.comparison}}.

ergm.info <- function(object) {
  coefs <- object$coef
  terms <- names(object$coef)
  coefmatrix <- summary(object)$coefs
  pval <- coefmatrix[, 4]
  signif.stars <- symnum(pval, corr = FALSE, na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                         symbols = c("***", "**", "*", ".", " "), legend = F)
  starredcoef <- paste0(format(coefs, digits = 3), signif.stars)

  ans <- list()
  count <- 1
  for (i in terms) {
    ans[i] <- starredcoef[count]
    count <- count + 1
  }
  ans$AIC <- format(summary(object)$aic, digits = 3)
  ans$BIC <- format(summary(object)$bic, digits = 3)
  ans
}


#' @title Compare ergm coefficients
#'
#' @description Takes in a list of multiple outputs from \code{\link{ergm.info}}, 
#' formatting it all into a table comparing up to 5 models.

coef.comparison <- function(coeflist) {
  nmod <- length(coeflist)
  if (nmod == 0)
    stop("No model summaries were passed to model.comparison")
  if (nmod > 5)
    stop("List of length greater than 5 passed to model.comparison")
  allterms <- c()
  for (j in 1:nmod) {
    allterms <- append(allterms, names(coeflist[[j]]))
  }
  allterms <- unique(allterms)
  allterms <- c(allterms[-which(allterms == "AIC")], "AIC")
  allterms <- c(allterms[-which(allterms == "BIC")], "BIC")
  mat <- matrix(nrow = length(allterms), ncol = nmod)

  for (k in 1:nmod) {
    row <- 1
    for (l in allterms) {
      if (is.null(coeflist[[k]][[l]])) {
        mat[row, k] <- "NA"
      } else {
        mat[row, k] <- coeflist[[k]][[l]]
      }
      row <- row + 1
    }
  }

  rownames(mat) <- allterms
  colnames(mat) <- paste0("Model", 1:nmod)
  return(print(mat, quote = FALSE))
}

stat.comparison <- function(statlist) {
  nmod <- length(statlist)
  if (nmod == 0)
    stop("No model summaries were passed to stat.comparison")
  if (nmod > 5)
    stop("List of length greater than 5 passed to stat.comparison")

  statvec <- c()
  for (j in 1:nmod) {
    for (k in 1:length(statlist[[j]])) {
      if (!(names(statlist[[j]][k]) %in% names(statvec))) {
        statvec <- append(statvec, statlist[[j]][k])
      }
    }
  }

  return(statvec)
}

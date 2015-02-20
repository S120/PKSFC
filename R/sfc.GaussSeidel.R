#' Run Gauss-Seidel algorithm.
#' 
#' Run the Gauss Seidel algorithm to solve the system of equations.
#' 
#' @param eqs the equations of the system.
#' @param tol the max change between itterations required for convergence.
#' @param maxIter the max number of itterations.
#' @param initialValues the initial values of the Gauss Seidel algorithm.
#' @param prev lag values to be used.
#' @return returns the solution of the Gauss Seidel algorithm for the system
#' 
#' @author Antoine Godin



sfc.GaussSeidel <- function(eqs, tol,maxIter, initialValues, prev) {
  vars <- vector(mode = "double", length = length(eqs))
  tols <- vector(mode = "double", length = length(eqs))
  tols[] <- tol
  vars[] <- Inf
  init <- initialValues
  iter<-0
  endogName <- names(eqs)
  matrixToPrint=matrix(0,nrow=1,ncol=length(init))
  for (j in 1:maxIter) {
    newinit <- init
    for (i in 1:length(eqs)) {
      eq <- eval(substitute(substitute(var, init), list(var = eqs[[i]])))
      eq <- eval(substitute(substitute(var, prev), list(var = eq)))
      newinit[endogName[i]] <- eval(eq)
      vars[i] <- abs((newinit[[endogName[i]]] - init[[endogName[i]]])/init[[endogName[i]]])
      if (is.na(vars[i])) {
        vars[i] <- 0
      }
    }
    matrixToPrint<-rbind(matrixToPrint,t(as.matrix(newinit)))
    init <- newinit
    iter<-j
    if (all(vars < tols)) {
      break
    }
  }
  res<-{}
  res$values<-init
  res$iterations<-iter
  return(res)
}

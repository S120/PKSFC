#' Set the years in a sfc object.
#' 
#' Set the years for which the sfc object is to examined over.
#' 
#' @param model an sfc object.
#' @param init intial date.
#' @param end end date.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export


sfc.setYear<-function(model=stop("Need a model"),init=stop("Need an initial year"),end=stop("Need and ending year")){
  model$time<-c(init:end)
  return(model)
}

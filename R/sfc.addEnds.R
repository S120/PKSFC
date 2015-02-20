#' A a list of endogenous variables.
#' 
#' Add a list of endogenous variables to the sfc object.
#' 
#' @inheritParams sfc.addEnd
#' @param list a list containing the endogenous variables, intial conditions, 
#' lags and description of the endogenous variable.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addEnds<-function(model=stop("Need a model"),list=stop("Need a list f variables")){
  for(i in 1:length(list)){
    var=list[[i]]$var
    init=list[[i]]$init
    lag=list[[i]]$lag
    desc=list[[i]]$desc
    if(is.null(var)){stop("each element of list has to contain an element var.")}
    if(is.null(init)){init=NA}
    if(is.null(lag)){lag=0}
    if(is.null(desc)){desc=""}
    model<-sfc.addEnd(model,var,init,lag,desc)
  }
  return(model)
}

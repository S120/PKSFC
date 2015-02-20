#' Add variables.
#' 
#' Add a series of variables to a sfc object.
#' 
#' @param model an sfc object.
#' @param list a list containing the variables to be added, initial conditions and descriptions.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addVars<-function(model=stop("Need a model"),list=stop("Need a list f variables")){
  for(i in 1:length(list)){
    var=list[[i]]$var
    init=list[[i]]$init
    desc=list[[i]]$desc
    if(is.null(var)){stop("each element of list has to contain an element var.")}
    if(is.null(init)){init=NA}
    if(is.null(desc)){desc=""}
    model<-sfc.addVar(model,var,init,desc)
  }
  return(model)
}

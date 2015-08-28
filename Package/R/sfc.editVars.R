#' Editing Variables.
#' 
#' Editin variables in an sfc object.
#' 
#' @param model an sfc model.
#' @param list a list containing lists of the indexs of variables to be changes,
#' the variables to be changed, the initial conditions, the lags and the descriptions of the
#' variables to be edited.
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin


sfc.editVars<-function(model=stop("Need a model"),list=stop("Need a list f variables")){
  for(i in 1:length(list)){
    ind=list[[i]]$ind
    var=list[[i]]$var
    init=list[[i]]$init
    desc=list[[i]]$desc
    if(is.null(ind)&&is.null(var)){stop("each element of list has to contain an element var or an element ind")}
    if(is.null(ind)){ind=NA}
    if(is.null(var)){var=NA}
    if(is.null(init)){init=NA}
    if(is.null(desc)){desc=NA}
    model<-sfc.editVar(model,ind,var,init,desc)
  }
  return(model)
}

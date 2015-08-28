#' Edit equations.
#' 
#' Edit a series of equations in an sfc object.
#' 
#' @param model an sfc object.
#' @param list a list containing lists of the indexs of equations to be changes,
#' the equations to be changed, the initial conditions, the lags and the descriptions of the
#' equations to be edited.
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin


sfc.editEqus<-function(model=stop("Need a model"),list=stop("Need a list f variables")){
  for(i in 1:length(list)){
    ind=list[[i]]$ind
    var=list[[i]]$var
    eq=list[[i]]$eq
    desc=list[[i]]$desc
    if(is.null(ind)&&is.null(var)){stop("each element of list has to contain an element var or an element ind")}
    if(is.null(ind)){ind=NA}
    if(is.null(var)){var=NA}
    if(is.null(eq)){eq=NA}
    if(is.null(desc)){desc=NA}
    model<-sfc.editEqu(model,ind,var,eq,desc)
  }
  return(model)
}

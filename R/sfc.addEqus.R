#' Adding Equations.
#' 
#' Add a series of equations to an sfc object.
#' 
#' @param model an sfc object.
#' @param list a list containing the equations, varaibles and descriptions to be added to the 
#' sfc object.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export


sfc.addEqus<-function(model=stop("Need a model"),list=stop("Need a list f variables")){
  for(i in 1:length(list)){
    var=list[[i]]$var
    eq=list[[i]]$eq
    desc=list[[i]]$desc
    if(is.null(var)){stop("each element of list has to contain an element var.")}
    if(is.null(eq)){stop("each element of list has to contain an element eq.")}
    if(is.null(desc)){desc=""}
    model<-sfc.addEqu(model,var,eq,desc)
  }
  return(model)
}

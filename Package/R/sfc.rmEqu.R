#' Remove equation.
#' 
#' Remove an equation from an sfc model.
#' 
#' @param model an sfc object. 
#' @param ind index of equation to be removed.
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin



sfc.rmEqu<-function(model=stop("Need a model"),ind=NA,var=NA){
  if(is.na(ind)&&is.na(var)){
    stop("Need an index or a varname")
  }
  if(is.null(model$equations)){
    stop("The model doesn't have equations yet")
  }else{
    if(is.na(ind)){
      ind=sfc.getIndex(model,eq=var)
    }
    indEnd<-sfc.getIndex(model,end=model$equations[ind,1])
    model<-sfc.rmEnd(model,indEnd)
    model$equations<-model$equations[-ind,]
  }
  model<-sfc.check(model)
  return(model)
}

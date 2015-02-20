#' Remove endogenous variable.
#' 
#' Remove an endogenous variable from an sfc model. 
#' 
#' @inheritParams sfc.editEnd
#' @return an sfc model.
#' 
#' @export
#' @author Antoine Godin


sfc.rmEnd<-function(model=stop("Need a model"),ind=stop("Need an index!")){
  if(is.null(model$endogenous)){
    stop("The model doesn't have endogenous variables yet")
  }else{
    indVar<-sfc.getIndex(model,var=model$endogenous[ind,1])
    model<-sfc.rmVar(model,indVar)
    model$endogenous<-model$endogenous[-ind,]
  }
  return(model)
}

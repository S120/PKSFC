#' Edit endogenous variable.
#' 
#' Edit an endogenous variable of a sfc object.
#' 
#' @inheritParams sfc.addEnd
#' @param ind index of endogenous variable to be changed.
#' @return sfc object.
#' 
#' @export
#' @author Antoine Godin


sfc.editEnd<-function(model=stop("Need a model"),ind=NA,var=NA,init=NA,lag=NA,desc=NA){
  if(is.na(ind)&&is.na(var)){
    stop("Need an index or a varname")
  }
  if(is.null(model$endogenous)){
    stop("The model doesn't have endogenous variables yet")
  }else{
    if(is.na(ind)){
      ind=sfc.getIndex(model,end=var)
      if(ind<0){
        stop("The model does not contain the endogenous variable ",var)
      }
    }
    if(!is.na(init)){model$endogenous[ind,2]<-init}
    if(!is.na(lag)){model$endogenous[ind,3]<-lag}
    if(!is.na(desc)){model$endogenous[ind,4]<-desc}
    indVar<-sfc.getIndex(model,var=model$endogenous[ind,1])
    if(indVar>0&&(!is.na(desc)||!is.na(init))){
      model<-sfc.editVar(model,ind=indVar,init=init,desc=desc)
    }
  }
  return(model)
}
